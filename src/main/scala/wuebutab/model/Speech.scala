package wuebutab

import monocle._
import monocle.syntax.all._

case class Speech(
  team: String,
  homeTeam: String,
  name: String,
  harmonizedName: String,
  round: Int,
  speechType: String,
  score: Double):

  def checkName(names: Names): Speech =
    val hname = names.harmonized.getOrElse(Name(this.homeTeam, this.name), this.name)
    this.focus(_.harmonizedName).replace(hname)

object Speech:

  def getHomeTeams(data: SeqTable, key: TableKey): Map[(String, Int), String] =
    val header = TableHeader(data.head.toVector.map(_.toString))
    data
      .filter(row => 
        val teamName = row(header.findLocalized("team", key)).toString
        teamName.startsWith("Swing") || teamName.startsWith("Mixed"))
      .filter(row => row(header.findLocalized("home_team", key)) != "")
      .map(row => 
          val homeTeam = row(header.findLocalized("home_team", key)).toString
          val name = row(header.findLocalized("name", key)).toString
          val round = row(header.findLocalized("round", key)).parseInt
          (name, round) -> homeTeam)
      .toMap

  def getAll(ballots: Vector[Ballot], names: Names, old: Option[SeqTable])(using config: Config): Vector[Speech] =
    val homeTeams = old.map(getHomeTeams(_, config.tableKeys.speeches)).getOrElse(Map())
    ballots.map(ballot =>
      val propSpeeches = for i <- (0 until 4).toVector yield Speech(
        ballot.prop,
        if ballot.prop.startsWith("Swing") | ballot.prop.startsWith("Mixed") 
          then homeTeams.getOrElse((ballot.propNames(i), ballot.round),"") 
          else ballot.prop,
        ballot.propNames(i),
        ballot.propNames(i),
        ballot.round,
        if i < 3 then "Main" else "Reply",
        ballot.propScores(i)
      ).checkName(names)
      val oppSpeeches = for i <- (0 until 4).toVector yield Speech(
        ballot.opp,
        if ballot.opp.startsWith("Swing") | ballot.opp.startsWith("Mixed") 
          then homeTeams.getOrElse((ballot.oppNames(i), ballot.round),"") 
          else ballot.opp,
        ballot.oppNames(i),
        ballot.oppNames(i),
        ballot.round,
        if i < 3 then "Main" else "Reply",
        ballot.oppScores(i)
      ).checkName(names)
      propSpeeches ++ oppSpeeches
    ).flatten.sortBy(speech => (speech.team, speech.name, speech.round))

  given t: Tabulatable[Speech] = new Tabulatable:

    def fields = Seq(
      TableField("team", _.team, false),
      TableField("home_team", _.homeTeam, false),
      TableField("name", _.name, false),
      TableField("harmonized_name", _.harmonizedName, false),
      TableField("round", _.round, true),
      TableField("type", _.speechType, false),
      TableField("score", _.score, true, Some(_.score.dpl(2))),
    )
