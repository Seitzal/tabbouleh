package wuebutab

import scala.jdk.CollectionConverters._
import ujson.Bool

opaque type TableHeader = Vector[String]

object TableHeader:
  def apply(headings: Vector[String]): TableHeader = headings

extension (tk: TableHeader)
  def findIgnoreCase(column: String): Int = tk.indexWhere(s => s.equalsIgnoreCase(column))
  def findLocalized(column: String, key: TableKey): Int =
    println(s"findLocalized: $column")
    val keycol = key(column)
    println(keycol)
    val loc = keycol.localizedTitle
    println(loc)
    val index = tk.indexOf(loc)
    println(index)
    tk.findIgnoreCase(key(column).localizedTitle)

type SeqTableContent = String | Int | Double | Boolean

extension (c: SeqTableContent)

  def parseInt: Int = c match
    case s: String => s.toInt
    case i: Int => i
    case d: Double => if d.isValidInt then d.toInt else throw NumberFormatException(s"$d is not an integer.")
    case true => 1
    case false => 0

  def asJavaObject: Object = c match
    case s: String => s
    case i: Int => Integer(i)
    case d: Double => java.lang.Double(d)
    case b: Boolean => java.lang.Boolean(b)

type SeqTable = Seq[Seq[SeqTableContent]]

case class TableField[T](
  name: String,
  f: T => SeqTableContent,
  align_right: Boolean = false)

trait Tabulatable[T]:
  def fields: Seq[TableField[T]]
  def names: Seq[String] = fields.map(_.name)

extension[T](t: T)(using tb: Tabulatable[T])
  def asTableRow: Seq[SeqTableContent] = tb.fields.map(field => field.f(t))

extension[T](ts: Seq[T])(using tb: Tabulatable[T])
  def asSeqTable: SeqTable = tb.names +: ts.map(_.asTableRow)

extension[T] (ts: java.util.List[java.util.List[T]])
  def asNestedSeq: Vector[Vector[String]] =
    ts.asScala.toVector.map(_.asScala.toVector.map(_.toString))

extension[T] (ts: Seq[T])
  def multiIndex(is: Seq[Int]): Seq[T] = for i <- is yield ts(i)

extension (t: SeqTable)

  def select(cols: Set[String]): SeqTable =
    val is = for i <- 0 until t.head.length if cols.contains(t.head(i).toString) yield i
    t.map(_.multiIndex(is))

  def select(cols: String*): SeqTable = t.select(cols.toSet)

  def asJavaNestedList: java.util.List[java.util.List[Object]] =
    t.map(_.map(_.asJavaObject).toList.asJava).toList.asJava

  def padRight: SeqTable =
    val maxLength = t.map(_.length).max
    t.map(row => row ++ (0 until maxLength - row.length).map(_ => ""))

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
