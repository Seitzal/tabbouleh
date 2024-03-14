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
  teams_prev: Vector[String] = Vector(),
  colleagues_prev: Vector[String] = Vector()):

  def penalty(t: Team): Int =
    if clashes.contains(t.name) then 10 else teams_prev.count(_ == t.name)

  def penalty(p: Pairing): Int = penalty(p.prop) + penalty(p.opp)

  def apply_panels(panels: Seq[Panel]): Judge =
    Panel.find_judge(this, panels) match
      case Some((panel, chair)) =>
        this.focus(_.teams_prev).modify(_ :+ panel.pairing.prop.name :+ panel.pairing.opp.name)
        .focus(_.times_chair).modify(n => if chair then n + 1 else n)
      case None => this.focus(_.teams_prev).modify(_ :+ "" :+ "")

  def updateForRound(round: SeqTable): Judge = 
    val found = for 
      row <- 0 until round.length
      col <- 0 until round(row).length
      if round(row)(col) == this.name
    yield (
      if col == 4 then 1 else 0,
      round(row).slice(1, 3),
      round(row).slice(4, 7).filter(name => name != this.name && name != ""))
    if !found.isEmpty then this
      .focus(_.times_chair).modify(_ + found.head._1)
      .focus(_.teams_prev).modify(_ ++ found.head._2)
      .focus(_.colleagues_prev).modify(_ ++ found.head._3)
    else this

  def updateForRounds(rounds: Seq[SeqTable]): Judge =
    if rounds.isEmpty then this
    else this.updateForRound(rounds.head).updateForRounds(rounds.tail)

object Judge:

  def fromRow(row: Vector[String]): Judge = Judge(
    row(0),
    if row(1) == "1" then true else false,
    row(2),
    row(3).toInt,
    0,
    Vector(row(4), row(5), row(6)).filter(s => !s.isEmpty)
  )

  def fetchAll(sheet: SpreadsheetHandle, range: String): Vector[Judge] =
    sheet.readRange(range).tail.map(fromRow)

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
      TableField("Seen", _.teams_prev.length, true))

    def to_csv(j: Judge): Map[String, String] = 
      Map(
        "Name" -> j.name,
        "Division" -> j.division.toString,
        "Rating" -> j.rating.toString,
        "Times Chair" -> j.times_chair.toString)
      ++ (0 until j.clashes.length).map(i => s"Clash ${i + 1}" -> j.clashes(i)).toMap
      ++ (0 until j.teams_prev.length).map(i => s"Team ${i + 1}" -> j.teams_prev(i)).toMap

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Name", "Division", "Rating", "Times Chair") 
      ++ keys.filter(_.startsWith("Clash")).toVector.sortBy(_.substring(5).toInt) 
      ++ keys.filter(_.startsWith("Team")).toVector.sortBy(_.substring(5).toInt) 
