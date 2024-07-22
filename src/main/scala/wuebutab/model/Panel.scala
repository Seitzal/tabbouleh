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

  given t: Tabulatable[Panel] = new Tabulatable:

    def fields = Seq(
      TableField("Div", _.pairing.division, false),
      TableField("MRS", _.pairing.mean_rank_score.dpl(6), false),
      TableField("Prop", _.pairing.prop.name, false),
      TableField("Opp", _.pairing.opp.name, false),
      TableField("Chair", _.chair.name, false),
      TableField("Panelist 1", _.panelist1.map(_.name).getOrElse(""), false),
      TableField("Panelist 2", _.panelist2.map(_.name).getOrElse(""), false))

