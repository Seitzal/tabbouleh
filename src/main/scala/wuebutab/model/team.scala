package wuebutab

import monocle._
import monocle.syntax.all._

case class Team(
  name: String,
  active: Boolean = true,
  division: String = "",
  wins: Int = 0,
  ballots: Int = 0,
  points: Double = 0d,
  side_pref: SidePref = SidePref(),
  previous_opponents: Vector[String] = Vector(),
  pull_ups: Int = 0):
  def rank_score: Double = wins + ballots * 1E-2 + points * 1E-6
  def apply_pairings(pairings: Seq[Pairing]): Team =
    Pairing.find_team(this, pairings) match
      case Some((opponent, side, pairing)) => 
        this.focus(_.previous_opponents).modify(_ :+ opponent.name)
          .focus(_.side_pref).modify(_.apply_pairing(pairing, side))
          .focus(_.pull_ups).modify(pu => if opponent.wins > this.wins then pu + 1 else pu)
      case None => this.focus(_.previous_opponents).modify(_ :+ "")

object Team:

  def apply(kv: Map[String, String]): Team = Team(
    kv("Name"),
    kv.getOrElse("Active", "true").toBoolean,
    kv.getOrElse("Division", ""),
    kv.getOrElse("Wins", "0").toInt,
    kv.getOrElse("Ballots", "0").toInt,
    kv.getOrElse("Points", "0").toDouble,
    SidePref(
      kv.getOrElse("Balance", "0").toInt,
      kv.getOrElse("Prep", "0").toInt,
      kv.getOrElse("Impr", "0").toInt),
    kv.keys.filter(_.startsWith("Opponent")).toVector.sorted.map(kv),
    kv.getOrElse("Pull-ups", "0").toInt)

  given Tabulatable[Team] = new Tabulatable:
    def fields = Seq(
      TableField("Name", _.name, false),
      TableField("Wins", _.wins, true),
      TableField("Ballots", _.ballots, true),
      TableField("Points", _.points.dpl(2), true),
      TableField("SP", _.side_pref.overall, true),
      TableField("SP(I)", _.side_pref.impr, true),
      TableField("SP(P)", _.side_pref.prep, true))

    def to_csv(t: Team): Map[String, String] = 
      Map(
        "Name" -> t.name,
        "Wins" -> t.wins.toString,
        "Ballots" -> t.ballots.toString,
        "Points" -> t.points.toString,
        "Rank Score" -> t.rank_score.toString,
        "Division" -> t.division,
        "Active" -> t.active.toString,
        "Balance" -> t.side_pref.overall.toString,
        "Prep" -> t.side_pref.prep.toString,
        "Impr" -> t.side_pref.impr.toString,
        "Pull-ups" -> t.pull_ups.toString)
      ++ (0 until t.previous_opponents.length).map(i => s"Opponent ${i + 1}" -> t.previous_opponents(i)).toMap

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Name", "Wins", "Ballots", "Points", "Rank Score", "Division", "Active", "Balance", "Prep", "Impr", "Pull-ups")
      ++ keys.filter(_.startsWith("Opponent")).toVector.sortBy(_.substring(9).toInt) 
