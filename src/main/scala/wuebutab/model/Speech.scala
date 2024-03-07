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

  def getHomeTeams(data: SeqTable): Map[(String, Int), String] =
    data
      .filter(row => row(0).startsWith("Swing") | row(0).startsWith("Mixed"))
      .filter(row => row(1) != "")
      .map(row => (row(2), row(4).toInt) -> row(1))
      .toMap

  def getAll(ballots: Vector[Ballot], names: Names, old: Option[SeqTable]): Vector[Speech] =
    val homeTeams = old.map(getHomeTeams).getOrElse(Map())
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
      TableField("Team", _.team, false),
      TableField("Home Team", _.homeTeam, false),
      TableField("Name", _.name, false),
      TableField("Harmonized Name", _.harmonizedName, false),
      TableField("Round", _.round.toString, true),
      TableField("Type", _.speechType, false),
      TableField("Score", _.score.dpl(2), true),
    )

    def to_csv(s: Speech) = ???

    def order_csv(keys: Set[String]): Seq[String] = ???
