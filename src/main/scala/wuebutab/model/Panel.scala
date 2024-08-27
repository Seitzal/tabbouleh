package wuebutab

case class Panel(
  pairing: Pairing,
  chair: Judge,
  panelist1: Option[Judge],
  panelist2: Option[Judge]):

  def toTableRow: Vector[String] = Vector(
    chair.name, 
    panelist1.map(_.name).getOrElse(""),
    panelist2.map(_.name).getOrElse(""))

object Panel:

  def find_judge(j: Judge, panels: Seq[Panel]): Option[(Panel, Boolean)] =
    panels.find(_.chair.name == j.name) match
      case Some(panel) => Some((panel, true))
      case None => 
        panels.find(p => 
          p.panelist1.map(_.name).getOrElse("") == j.name 
          || p.panelist2.map(_.name).getOrElse("") == j.name)
        .map((_, false))
  
  def updateRemote(panels: Vector[Panel], round: Int)
      (using remote: SpreadsheetHandle, config: Config): Unit =
    val key = config.tableKeys.panels
    val sheetName = config.sheetNames.round_prefix + round.toString
    if !remote.sheetExists(sheetName) then remote.createSheet(sheetName)
    remote.writeRange(
      s"$sheetName!E1", 
      panels.asSeqTable(key).select(key("chair"), key("panelist1"), key("panelist2")))

  given t: Tabulatable[Panel] = new Tabulatable:

    def fields = Seq(
      TableField("division", _.pairing.division, false),
      TableField("mean_rank_score", _.pairing.mean_rank_score, true, Some(_.pairing.mean_rank_score.dpl(6))),
      TableField("prop", _.pairing.prop.name, false),
      TableField("opp", _.pairing.opp.name, false),
      TableField("chair", _.chair.name, false),
      TableField("panelist1", _.panelist1.map(_.name).getOrElse(""), false),
      TableField("panelist2", _.panelist2.map(_.name).getOrElse(""), false))

