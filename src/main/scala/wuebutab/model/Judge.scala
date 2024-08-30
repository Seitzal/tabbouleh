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
      case Some((panel, chair)) => this
        .focus(_.teams_prev)
        .modify(_ :+ panel.pairing.prop.name :+ panel.pairing.opp.name)
        .focus(_.times_chair)
        .modify(n => if chair then n + 1 else n)
      case None => this
        .focus(_.teams_prev)
        .modify(_ :+ "" :+ "")

  def updateForRound(round: Vector[Vector[String]])(using config: Config): Judge = 
    val header = TableHeader(round.head)
    val key =  config.tableKeys.panels
    val found = for 
      row <- 0 until round.length
      col <- 0 until round(row).length
      if round.isDefinedAt(row) 
      && round(row).isDefinedAt(col) 
      && round(row)(col) == this.name
    yield (
      if col == header.findLocalized("chair", key) then 1 else 0,
      round(row).multiIndex(Seq(
        header.findLocalized("prop", key), 
        header.findLocalized("opp", key)
      )),
      round(row).multiIndex(Seq(
        header.findLocalized("chair", key), 
        header.findLocalized("panelist1", key),
        header.findLocalized("panelist2", key)
      )).filter(name => name != this.name && name != ""))
    if !found.isEmpty then this
      .focus(_.times_chair).modify(_ + found.head._1)
      .focus(_.teams_prev).modify(_ ++ found.head._2)
      .focus(_.colleagues_prev).modify(_ ++ found.head._3)
    else this

  def updateForRounds(rounds: Seq[Vector[Vector[String]]])(using config: Config): Judge =
    if rounds.isEmpty then this
    else this.updateForRound(rounds.head).updateForRounds(rounds.tail)

object Judge:

  def apply(row: Vector[String], header: TableHeader)(using config: Config): Judge = 
    val key = config.tableKeys.judges
    Judge(
      row(header.findLocalized("name", key)),
      row(header.findLocalized("active", key)).toBoolean,
      row(header.findLocalized("division", key)),
      row(header.findLocalized("rating", key)).toInt,
      0,
      row.multiIndex(header.findLocalizedMulti("clash_prefix", key)).toVector.filter(s => !s.isEmpty)
    )

  def fetchAll(using remote: SpreadsheetHandle, config: Config): Vector[Judge] =
    val table = remote.readRange(config.sheetNames.judges)
    val header = TableHeader(table.head)
    table.tail.map(row => Judge(row, header))

  given t: Tabulatable[Judge] = new Tabulatable:

    def fields = Seq(
      TableField("name", _.name),
      TableField("active", _.active),
      TableField("division", _.division),
      TableField("rating", _.rating, true),
      TableField("times_chair", _.times_chair, true),
      TableField("clashes", _.clashes.length, true),
      TableField("seen", _.teams_prev.length, true))
