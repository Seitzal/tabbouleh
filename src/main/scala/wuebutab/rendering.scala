package wuebutab

extension(d: Double)
  def dpl(places: Int): String = s"%.${places}f".format(d)

def whitespace(n: Int, acc: String = ""): String =
  if n == 0 then acc else whitespace(n - 1, acc + " ")

def pad(xs: Seq[String], left: Boolean = false): Seq[String] =
  val max_length = xs.map(_.length).max
  if left 
    then xs.map(x => whitespace(max_length - x.length) + x)
    else xs.map(x => x + whitespace(max_length - x.length))

case class TableField[T](
  name: String,
  f: T => Any,
  align_right: Boolean = false)

trait Tabulatable[T]:
  def fields: Seq[TableField[T]]

def render_table[T](ts: Seq[T])(using tb: Tabulatable[T]): String =
  val cols = tb.fields.map(field => pad(ts.map(field.f).map(_.toString).prepended(field.name), field.align_right))
  (0 to ts.length).map(i => cols.map(_.apply(i)).mkString("  ")).mkString("\n") + "\n"

given Tabulatable[Team] = new Tabulatable:
  def fields = Seq(
    TableField("Name", _.name, false),
    TableField("Wins", _.wins, true),
    TableField("Ballots", _.ballots, true),
    TableField("Points", _.points.dpl(2), true),
    TableField("SP", _.side_pref.overall, true),
    TableField("SP(I)", _.side_pref.impr, true),
    TableField("SP(P)", _.side_pref.prep, true))

given Tabulatable[Pairing] = new Tabulatable:
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

given Tabulatable[Judge] = new Tabulatable:
  def fields = Seq(
    TableField("Name", _.name, false),
    TableField("Active", _.active, false),
    TableField("Division", _.division, false),
    TableField("Rating",)
  )
