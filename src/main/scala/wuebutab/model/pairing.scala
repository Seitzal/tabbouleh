package wuebutab

import scala.util.Random

case class Pairing(
  prop: Team,
  opp: Team,
  dt: DebateType,
  weight: Double):
  lazy val ran = Random.nextBoolean()
  def rematch: Boolean = prop.previous_opponents.contains(opp.name)
  def division: String = prop.division
  def mean_rank_score: Double = (prop.rank_score + opp.rank_score) / 2

object Pairing:

  def init(t1: Team, t2: Team, dt: DebateType, weight: Double): Pairing =
    val prop: Team =
      if t1.side_pref(dt) == t2.side_pref(dt) && Random.nextBoolean() then t2
      else if t1.side_pref(dt) > t2.side_pref(dt) then t2
      else t1
    val opp: Team = if prop == t1 then t2 else t1
    Pairing(prop, opp, dt, weight)

  def apply(kv: Map[String, String], ts: Seq[Team], dt: DebateType): Pairing = Pairing(
    ts.find(_.name == kv("Proposition Team")).get,
    ts.find(_.name == kv("Opposition Team")).get,
    dt,
    0d)

  def find_team(t: Team, pairings: Seq[Pairing]): Option[(Team, Side, Pairing)] =
    pairings.find(_.prop.name == t.name) match
      case Some(pairing) => Some((pairing.opp, Side.Proposition, pairing))
      case None => pairings.find(_.opp.name == t.name).map(p => (p.prop, Side.Opposition, p)) 

  given t: Tabulatable[Pairing] = new Tabulatable:

    def fields = Seq(
      TableField("MRS", _.mean_rank_score.dpl(6), true),
      TableField("Weight", _.weight.dpl(6), true),
      TableField("Prop", _.prop.name, false),
      TableField("W", _.prop.wins, true),
      TableField("B", _.prop.ballots, true),
      TableField("P", _.prop.points.dpl(2), true),
      TableField("SP",  p => p.prop.side_pref(p.dt), true),
      TableField("Opp", _.opp.name, false),
      TableField("W", _.opp.wins, true),
      TableField("B", _.opp.ballots, true),
      TableField("P", _.opp.points.dpl(2), true),
      TableField("SP",  p => p.opp.side_pref(p.dt), true))

    def to_csv(p: Pairing): Map[String, String] = Map(
        "MRS" -> p.mean_rank_score.toString,
        "Weight" -> p.weight.toString,
        "Proposition Team" -> p.prop.name,
        "Opposition Team" -> p.opp.name)

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("MRS", "Weight", "Proposition Team", "Opposition Team")
