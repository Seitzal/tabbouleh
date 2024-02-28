package wuebutab

import scala.jdk.CollectionConverters._

type SeqTable = Seq[Seq[String]]

case class TableField[T](
  name: String,
  f: T => Any,
  align_right: Boolean = false)

trait Tabulatable[T]:
  def fields: Seq[TableField[T]]
  def names: Seq[String] = fields.map(_.name)
  def to_csv(t: T): Map[String, String]
  def order_csv(keys: Set[String]): Seq[String]

extension[T](t: T)(using tb: Tabulatable[T])
  def asSeq: Seq[Any] = tb.fields.map(field => field.f(t))
  def asStringSeq: Seq[String] = t.asSeq.map(_.toString)

extension[T](ts: Seq[T])(using tb: Tabulatable[T])
  def asSeqTable: SeqTable = tb.names +: ts.map(_.asStringSeq)

extension[T] (ts: java.util.List[java.util.List[T]])
  def asNestedSeq: Vector[Vector[String]] =
    ts.asScala.toVector.map(_.asScala.toVector.map(_.toString))

extension[T] (ts: Seq[T])
  def multiIndex(is: Seq[Int]): Seq[T] = for i <- is yield ts(i)

extension (t: SeqTable)
  def select(cols: Set[String]): SeqTable =
    val is = for i <- 0 until t.head.length if cols.contains(t.head(i)) yield i
    t.map(_.multiIndex(is))
  def select(cols: String*): SeqTable = t.select(cols.toSet)
  def asJavaNestedList: java.util.List[java.util.List[Object]] =
    t.map(_.toList.asJava).toList.asJava

extension(d: Double)
  def dpl(places: Int): String = s"%.${places}f".format(d)

def whitespace(n: Int, acc: String = ""): String =
  if n == 0 then acc else whitespace(n - 1, acc + " ")

def pad(xs: Seq[String], left: Boolean = false): Seq[String] =
  val max_length = xs.map(_.length).max
  if left 
    then xs.map(x => whitespace(max_length - x.length) + x)
    else xs.map(x => x + whitespace(max_length - x.length))

def render_table[T](ts: Seq[T])(using tb: Tabulatable[T]): String =
  val cols = tb.fields.map(field => pad(ts.map(field.f).map(_.toString).prepended(field.name), field.align_right))
  (0 to ts.length).map(i => cols.map(_.apply(i)).mkString("  ")).mkString("\n") + "\n"
