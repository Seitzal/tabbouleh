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

  def init(t1: Team, t2: Team, dt: DebateType, weight: Double, round: Int): Pairing =
    val prop = (t1.sidelock.get(round), t2.sidelock.get(round)) match
      case (Some(Side.Proposition), _) => t1
      case (Some(Side.Opposition), _) => t2
      case (None, Some(Side.Proposition)) => t2
      case (None, Some(Side.Opposition)) => t1
      case _ =>
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
      TableField("division", _.division, false),
      TableField("mean_rank_score", _.mean_rank_score, true, Some(_.mean_rank_score.dpl(6))),
      TableField("weight", _.weight, true, Some(_.weight.dpl(6))),
      TableField("prop", _.prop.name, false),
      TableField("wins_prop", _.prop.wins, true),
      TableField("ballots_prop", _.prop.ballots, true),
      TableField("points_prop", _.prop.points, true, Some(_.prop.points.dpl(2))),
      TableField("sidepref_prop",  p => p.prop.side_pref(p.dt), true),
      TableField("opp", _.opp.name, false),
      TableField("wins_opp", _.opp.wins, true),
      TableField("ballots_opp", _.opp.ballots, true),
      TableField("points_opp", _.opp.points, true, Some(_.opp.points.dpl(2))),
      TableField("sidepref_opp",  p => p.opp.side_pref(p.dt), true))

  def updateRemote(pairings: Vector[Pairing], round: Int)(using remote: SpreadsheetHandle, config: Config): Unit =
    val key = config.tableKeys.pairings
    val sheetName = config.sheetNames.round_prefix + round.toString
    if !remote.sheetExists(sheetName) then remote.createSheet(sheetName)
    remote.writeRange(
      s"$sheetName!A1", 
      pairings.asSeqTable(key).select(key("division"), key("prop"), key("opp")))
    