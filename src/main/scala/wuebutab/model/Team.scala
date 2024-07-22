package wuebutab

import monocle._
import monocle.syntax.all._

case class TeamMeta(
  division: String,
  active: Boolean,
  pull_ups: Int,
  sidelock: Map[Int, Side])

object TeamMeta:

  def apply(row: Vector[String], header: TableHeader)(using config: Config): (String, TeamMeta) =
    val key = config.tableKeys.teams
    val sidelocked_rounds = header.suffixes("sidelock_prefix", key).map(_.toInt)
    val sides = row.multiIndex(header.findLocalizedMulti("sidelock_prefix", key)).map(Side.fromSymbol)
    val sidelock = 
      sidelocked_rounds.zip(sides)
      .filter(_._2.isDefined)
      .map((round, sideOpt) => (round, sideOpt.get))
      .toMap
    row(header.findLocalized("team", key)) -> TeamMeta(
      row(header.findLocalized("division", key)).ifEmpty("univ."),
      row(header.findLocalized("active", key)).ifEmpty("false").toBoolean,
      row(header.findLocalized("pull_ups", key)).ifEmpty("0").toInt,
      sidelock
    )

  def fetchAll(using remote: SpreadsheetHandle, config: Config): Map[String, TeamMeta] =
    if !remote.rangeExists(config.sheetNames.teams) then return Map()
    val table = remote.readRange(config.sheetNames.teams)
    table.tail.map(row => TeamMeta(row, TableHeader(table.head))).toMap

case class Team(
  name: String,
  wins: Int,
  ballots: Int,
  points: Double,
  side_pref: SidePref,
  previous_opponents: Vector[String],
  meta: TeamMeta):

  def division = meta.division
  def active = meta.active
  def pull_ups = meta.pull_ups
  def sidelock = meta.sidelock
  def rank_score: Double = wins + ballots * 1E-2 + points * 1E-6

  def apply_pairings(pairings: Seq[Pairing], round: Int): Team =
    Pairing.find_team(this, pairings) match
      case Some((opponent, side, pairing)) => 
        val before_sides = 
          this.focus(_.previous_opponents).modify(_ :+ opponent.name)
            .focus(_.meta.pull_ups).modify(pu => if opponent.wins > this.wins then pu + 1 else pu)
        if sidelock.isDefinedAt(round) then before_sides else
          before_sides.focus(_.side_pref).modify(_.apply_pairing(pairing, side))   
      case None => this

object Team:

  def apply(
    team: String, 
    debateResults: Vector[DebateResults], 
    rounds: Vector[Round],
    meta: Map[String, TeamMeta]): Team =

    val regularWins = debateResults.filter(d => d.regularMatchFor(team) && d.winner == team).length
    val regularBallots = debateResults.filter(_.regularMatchFor(team)).map(_.ballots(team)).sum
    val regularPoints = debateResults.filter(_.regularMatchFor(team)).map(_.points(team)).sum
    val regularN = debateResults.filter(_.regularMatchFor(team)).length

    val mixedN = debateResults.filter(_.mixedMatchFor(team)).length
    val naSwingN = debateResults.filter(_.naSwingFor(team)).length

    val imputedWins = mixedN + Math.round((regularWins.toFloat / regularN) * naSwingN)
    val mixedBallots = mixedN * (if regularBallots >= regularN * (3d / 2) then 3 else 2)
    val naSwingNBallots = (regularBallots.toFloat / regularN).toInt
    val imputedBallots = mixedBallots + naSwingNBallots * naSwingN
    val imputedPoints = (mixedN + naSwingN) * (regularPoints / regularN)

    val prepRounds = rounds.filter(_.debateType == DebateType.Prepared).map(_.roundNo)
    val imprRounds = rounds.filter(_.debateType == DebateType.Impromptu).map(_.roundNo)
    val propImprN = debateResults.filter(_.prop == team).map(_.round).filter(imprRounds.contains).length
    val oppImprN = debateResults.filter(_.opp == team).map(_.round).filter(imprRounds.contains).length
    val propPrepN = debateResults.filter(_.prop == team).map(_.round).filter(prepRounds.contains).length
    val oppPrepN = debateResults.filter(_.opp == team).map(_.round).filter(prepRounds.contains).length

    val sidePref = SidePref(
      (propPrepN + propImprN - oppPrepN - oppImprN) / 2,
      (propPrepN - oppPrepN) / 2,
      (propImprN - oppImprN) / 2)

    val previous_opponents = (
      debateResults.filter(_.prop == team).map(_.opp) ++
      debateResults.filter(_.opp == team).map(_.prop)
    ).distinct

    new Team(
      team,
      regularWins + imputedWins,
      regularBallots + imputedBallots,
      regularPoints + imputedPoints,
      sidePref,
      previous_opponents,
      meta.getOrElse(team, TeamMeta("univ.", true, 0, Map())))
    
  def getAll(debateResults: Vector[DebateResults], rounds: Vector[Round], meta: Map[String, TeamMeta]) =
    for team <- (debateResults.map(_.winner) ++ debateResults.map(_.loser) ++ meta.keys).distinct
    yield Team(team, debateResults, rounds, meta) 

extension(teams: Vector[Team])

  def updateRemote()(using remote: SpreadsheetHandle, config: Config): Unit =
    val sheet = config.sheetNames.teams
    if !remote.sheetExists(sheet) then remote.createSheet(sheet)
    remote.writeRange(s"$sheet!A1", 
      Vector(config.tableKeys.teams("rank")) +: 
      (for i <- 1 to teams.length yield Vector(i)).toVector)
    remote.writeRange(s"$sheet!B1", teams.asSeqTable(config.tableKeys.teams))
    val sidelocked_rounds = teams.map(_.meta.sidelock.keySet).reduce(_.union(_)).toVector.sorted
    val sidelocks_table = 
      sidelocked_rounds.map(config.tableKeys.teams("sidelock_prefix") + _) +:
      teams.map(team => sidelocked_rounds.map(round => team.meta.sidelock.get(round).map(_.symbol).getOrElse("-")))
    remote.writeRange(s"$sheet!L1", sidelocks_table)

given t: Tabulatable[Team] = new Tabulatable:

  def fields = Seq(
    TableField("team", _.name),
    TableField("wins", _.wins, true),
    TableField("ballots", _.ballots, true),
    TableField("points", _.points, true, Some(_.points.dpl(2))),
    TableField("sidepref", _.side_pref.overall, true),
    TableField("sidepref_prep", _.side_pref.prep, true),
    TableField("sidepref_impr", _.side_pref.impr, true),
    TableField("pull_ups", _.meta.pull_ups, true),
    TableField("active", _.meta.active),
    TableField("division", _.meta.division))
