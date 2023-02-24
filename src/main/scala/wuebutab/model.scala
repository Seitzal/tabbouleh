package wuebutab

import scala.util.Random
import monocle._
import monocle.syntax.all._
import cats.implicits._
import simulacrum.op

case class Tab(
  name: String = "new tab",
  teams: Vector[Team] = Vector()):
  def active_teams = teams.filter(_.active)
  def apply_pairings(pairings: Seq[Pairing]): Tab =
    this.focus(_.teams).modify(_.map(_.apply_pairings(pairings)))

enum Side(val symbol: String):
  case Proposition extends Side("P")
  case Opposition extends Side("O")

enum DebateType(val symbol: String):
  case Impromptu extends DebateType("Impr")
  case Prepared extends DebateType("Prep")

case class SidePref(
  overall: Int = 0,
  prep: Int = 0,
  impr: Int = 0):
  def apply(dt: DebateType): Int =
    if dt == DebateType.Prepared && prep != 0 then prep
    else if dt == DebateType.Impromptu && impr != 0 then impr
    else overall
  override def toString(): String = s"$overall $prep $impr"
  def apply_pairing(p: Pairing, s: Side): SidePref = (p.dt, s) match   
    case (DebateType.Prepared, Side.Proposition) => SidePref(overall + 2, prep + 2, impr)
    case (DebateType.Prepared, Side.Opposition) => SidePref(overall - 2, prep - 2, impr)
    case (DebateType.Impromptu, Side.Proposition) => SidePref(overall + 2, prep, impr + 2)
    case (DebateType.Impromptu, Side.Opposition) => SidePref(overall - 2, prep, impr - 2)
    
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
    find_team(this, pairings) match
      case Some((opponent, side, pairing)) => 
        this.focus(_.previous_opponents).modify(_ :+ opponent.name)
          .focus(_.side_pref).modify(_.apply_pairing(pairing, side))
          .focus(_.pull_ups).modify(pu => if opponent.wins > this.wins then pu + 1 else pu)
      case None => this.focus(_.previous_opponents).modify(_ :+ "")

object Team:
  def apply(kv: Map[String, String]): Team = Team(
    kv("Team"),
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

case class Pairing(
  t1: Team,
  t2: Team,
  dt: DebateType,
  weight: Double):
  lazy val ran = Random.nextBoolean()
  def prop: Team = 
    if t1.side_pref(dt) == t2.side_pref(dt) && ran then t2
    else if t1.side_pref(dt) > t2.side_pref(dt) then t2
    else t1
  def opp: Team = if prop == t1 then t2 else t1
  def rematch: Boolean = t1.previous_opponents.contains(t2.name)
  def mean_rank_score: Double = (t1.rank_score + t2.rank_score) / 2

case class Judge(
  name: String,
  active: Boolean = true,
  division: String = "",
  rating: Int = 1,
  clashes: Vector[String] = Vector(),
  previously_seen: Vector[String] = Vector()):
  def penalty(t: Team): Int =
    if clashes.contains(t.name) then 10 else previously_seen.count(_ == t.name)
  def penalty(p: Pairing): Int = penalty(p.t1) + penalty(p.t2)

object Judge:
  def apply(kv: Map[String, String]): Judge = Judge(
    kv("Name"),
    kv.getOrElse("Active", "true").toBoolean,
    kv.getOrElse("Division", ""),
    kv.getOrElse("Rating", "0").toInt,
    kv.keys.filter(_.startsWith("Clash")).toVector.sorted.map(kv),
    kv.keys.filter(_.startsWith("Team")).toVector.sorted.map(kv))

case class Panel(
  pairing: Pairing,
  chair: Judge,
  panelist1: Option[Judge],
  panelist2: Option[Judge])

def find_team(t: Team, pairings: Seq[Pairing]): Option[(Team, Side, Pairing)] =
  pairings.find(_.prop.name == t.name) match
    case Some(pairing) => Some((pairing.opp, Side.Proposition, pairing))
    case None => pairings.find(_.opp.name == t.name).map(p => (p.prop, Side.Opposition, p))
