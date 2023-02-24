package wuebutab

import monocle._
import monocle.syntax.all._

case class Tab(
  name: String = "new tab",
  teams: Vector[Team] = Vector(),
  judges: Vector[Judge] = Vector()):
  def active_teams = teams.filter(_.active)
  def apply_pairings(pairings: Seq[Pairing]): Tab =
    this.focus(_.teams).modify(_.map(_.apply_pairings(pairings)))
  def apply_panels(panels: Seq[Panel]): Tab =
    this.focus(_.judges).modify(_.map(_.apply_panels(panels)))
