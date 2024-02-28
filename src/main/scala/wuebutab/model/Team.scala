package wuebutab

import monocle._
import monocle.syntax.all._

case class TeamMeta(
  division: String,
  active: Boolean,
  pull_ups: Int,
  sidelock: Map[Int, Side])

object TeamMeta:

  def apply(tableFields: Map[String, String]) =
    new TeamMeta(
      tableFields.getOrElse("Division", "univ."),
      tableFields.getOrElse("Active", "1") != "0",
      tableFields.getOrElse("pull_ups", "0").toInt,
      tableFields
        .filter((k, v) => k.startsWith("Side "))
        .map((k, v) => k.drop(5).toInt -> Side.fromSymbol(v))
    )

  def getAll(table: Vector[Vector[String]]) =
    val nameColumn = table.head.indexOf("Team")
    (for row <- table.tail yield row(nameColumn) -> TeamMeta(table.head.zip(row).toMap)).toMap

  def fetchAll(sheet: SpreadsheetHandle, range: String) =
    getAll(sheet.readRange(range))

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

    val regularWins = debateResults.filter(_.winner == team).length
    val regularBallots = debateResults.map(_.ballots(team)).sum
    val regularPoints = debateResults.map(_.points(team)).sum
    val regularN = debateResults.filter(_.regularMatchFor(team)).length

    val mixedN = debateResults.filter(_.mixedMatchFor(team)).length
    val naSwingN = debateResults.filter(_.naSwingFor(team)).length

    val imputedWins = mixedN + Math.round((regularWins.toFloat / regularN) * naSwingN)
    val mixedBallots = mixedN * (if regularBallots >= regularN * (3d / 2) then 3 else 2)
    val naSwingNBallots = (regularBallots.toFloat / regularN).toInt
    val imputedBallots = mixedBallots + naSwingNBallots
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
      debateResults.filter(_.opp == team).map(_.opp)
    ).distinct

    new Team(
      team,
      regularWins + imputedWins,
      regularBallots + imputedBallots,
      regularPoints + imputedPoints,
      sidePref,
      previous_opponents,
      meta.getOrElse(team, TeamMeta("univ.", false, 0, Map())))

  def apply(kv: Map[String, String]): Team = new Team(
    kv("Name"),
    kv.getOrElse("Wins", "0").toInt,
    kv.getOrElse("Ballots", "0").toInt,
    kv.getOrElse("Points", "0").toDouble,
    SidePref(
      kv.getOrElse("Balance", "0").toInt,
      kv.getOrElse("Prep", "0").toInt,
      kv.getOrElse("Impr", "0").toInt),
    kv.keys.filter(_.startsWith("Opponent")).toVector.sorted.map(kv),
    TeamMeta(
      kv.getOrElse("Division", ""),
      kv.getOrElse("Active", "true").toBoolean,
      kv.getOrElse("Pull-ups", "0").toInt,
      kv.filter((k, v) => k.startsWith("Side"))
        .map((k, v) => k.drop(5).toInt -> Side.fromSymbol(v))))
    
  def getAll(debateResults: Vector[DebateResults], rounds: Vector[Round], meta: Map[String, TeamMeta]) =
    for team <- (debateResults.map(_.winner) ++ debateResults.map(_.loser) ++ meta.keys).distinct 
    yield Team(team, debateResults, rounds, meta) 

  given t: Tabulatable[Team] = new Tabulatable:

    def fields = Seq(
      TableField("Team", _.name, false),
      TableField("Wins", _.wins.toString, true),
      TableField("Ballots", _.ballots.toString, true),
      TableField("Points", _.points.dpl(2), true),
      TableField("SP", _.side_pref.overall.toString, true),
      TableField("P", _.side_pref.prep.toString, true),
      TableField("I", _.side_pref.impr.toString, true))

    def to_csv(tr: Team): Map[String, String] = Map(
      "Team" -> tr.name,
      "Wins" -> tr.wins.toString,
      "Ballots" -> tr.ballots.toString,
      "Points" -> tr.points.toString,
      "SidePref" -> tr.side_pref.overall.toString,
      "SidePrefPrep" -> tr.side_pref.prep.toString,
      "SidePrefImpr" -> tr.side_pref.impr.toString)

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Team", "Wins", "Ballots", "Points", "SidePref", "SidePrefPrep", "SidePrefImpr")
