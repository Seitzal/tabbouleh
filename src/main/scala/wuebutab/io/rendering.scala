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
  def to_csv(t: T): Map[String, String]
  def order_csv(keys: Set[String]): Seq[String]

def render_table[T](ts: Seq[T])(using tb: Tabulatable[T]): String =
  val cols = tb.fields.map(field => pad(ts.map(field.f).map(_.toString).prepended(field.name), field.align_right))
  (0 to ts.length).map(i => cols.map(_.apply(i)).mkString("  ")).mkString("\n") + "\n"
