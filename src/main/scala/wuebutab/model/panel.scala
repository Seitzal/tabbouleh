package wuebutab

case class Panel(
  pairing: Pairing,
  chair: Judge,
  panelist1: Option[Judge],
  panelist2: Option[Judge])

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
      TableField("Weight", _.pairing.weight.dpl(6), true),
      TableField("Prop", _.pairing.prop.name, false),
      TableField("Opp", _.pairing.opp.name, false),
      TableField("Chair", _.chair.name, false),
      TableField("Panelist 1", _.panelist1.map(_.name).getOrElse(""), false),
      TableField("Panelist 2", _.panelist2.map(_.name).getOrElse(""), false))

    def to_csv(p: Panel): Map[String, String] = Map(
        "Division" -> p.pairing.division,
        "Proposition Team" -> p.pairing.prop.name,
        "Opposition Team" -> p.pairing.opp.name,
        "Chair" -> p.chair.name,
        "Panelist 1" -> p.panelist1.map(_.name).getOrElse(""),
        "Panelist 2" -> p.panelist2.map(_.name).getOrElse(""))

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Division", "Proposition Team", "Opposition Team", "Chair", "Panelist 1", "Panelist 2")
