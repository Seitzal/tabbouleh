package wuebutab

import monocle._
import monocle.syntax.all._

case class Judge(
  name: String,
  active: Boolean = true,
  division: String = "",
  rating: Int = 1,
  times_chair: Int = 0,
  clashes: Vector[String] = Vector(),
  previously_seen: Vector[String] = Vector()):
  def penalty(t: Team): Int =
    if clashes.contains(t.name) then 10 else previously_seen.count(_ == t.name)
  def penalty(p: Pairing): Int = penalty(p.prop) + penalty(p.opp)
  def apply_panels(panels: Seq[Panel]): Judge =
    Panel.find_judge(this, panels) match
      case Some((panel, chair)) =>
        this.focus(_.previously_seen).modify(_ :+ panel.pairing.prop.name :+ panel.pairing.opp.name)
        .focus(_.times_chair).modify(n => if chair then n + 1 else n)
      case None => this.focus(_.previously_seen).modify(_ :+ "" :+ "")

object Judge:

  def apply(kv: Map[String, String]): Judge = Judge(
    kv("Name"),
    kv.getOrElse("Active", "true").toBoolean,
    kv.getOrElse("Division", ""),
    kv.getOrElse("Rating", "0").toInt,
    kv.getOrElse("Times Chair", "0").toInt,
    kv.keys.filter(_.startsWith("Clash")).toVector.sorted.map(kv).filter(_ != ""),
    kv.keys.filter(_.startsWith("Team")).toVector.sorted.map(kv))

  given t: Tabulatable[Judge] = new Tabulatable:

    def fields = Seq(
      TableField("Name", _.name, false),
      TableField("Active", _.active, false),
      TableField("Division", _.division, false),
      TableField("Rating", _.rating, true),
      TableField("Times Chair", _.times_chair, true),
      TableField("Clashes", _.clashes.length, true),
      TableField("Seen", _.previously_seen.length, true))

    def to_csv(j: Judge): Map[String, String] = 
      Map(
        "Name" -> j.name,
        "Division" -> j.division.toString,
        "Rating" -> j.rating.toString,
        "Times Chair" -> j.times_chair.toString)
      ++ (0 until j.clashes.length).map(i => s"Clash ${i + 1}" -> j.clashes(i)).toMap
      ++ (0 until j.previously_seen.length).map(i => s"Team ${i + 1}" -> j.previously_seen(i)).toMap

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Name", "Division", "Rating", "Times Chair") 
      ++ keys.filter(_.startsWith("Clash")).toVector.sortBy(_.substring(5).toInt) 
      ++ keys.filter(_.startsWith("Team")).toVector.sortBy(_.substring(5).toInt) 
