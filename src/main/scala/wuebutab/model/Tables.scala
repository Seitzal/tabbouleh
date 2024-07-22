package wuebutab

import scala.jdk.CollectionConverters._
import upickle.default.ReadWriter

//--- TableKey ---

type TableKey = Map[String, String]

//--- TableHeader ---

opaque type TableHeader = Vector[String]

object TableHeader:
  def apply(headings: Vector[String]): TableHeader = headings

extension (header: TableHeader)
  def findIgnoreCase(column: String): Int = header.indexWhere(s => s.equalsIgnoreCase(column))
  def findLocalized(column: String, key: TableKey): Int =
    header.findIgnoreCase(key(column))
  def findLocalizedMulti(prefix: String, key: TableKey): Vector[Int] =
    (for i <- 0 until header.length if header(i).startsWith(key(prefix)) yield i).toVector
  def suffixes(prefix: String, key: TableKey): Vector[String] =
    header.filter(_.startsWith(key(prefix))).map(_.drop(key(prefix).length))

//--- SeqTableContent ---

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

//--- SeqTable ---

type SeqTable = Seq[Seq[SeqTableContent]]

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

//--- TableField ---

case class TableField[T](
  name: String,
  f: T => SeqTableContent,
  align_right: Boolean = false,
  fPlainText: Option[T => String] = None):

  def plainText: T => String = fPlainText match
    case Some(f) => f
    case None => t => f(t).toString

//--- Tabulatable ---

trait Tabulatable[T]:
  def fields: Seq[TableField[T]]
  def names: Seq[String] = fields.map(_.name)

extension[T](t: T)(using tb: Tabulatable[T])
  def asTableRow: Seq[SeqTableContent] = tb.fields.map(field => field.f(t))

extension[T](ts: Seq[T])(using tb: Tabulatable[T])

  def asSeqTable(tk: TableKey): SeqTable = 
    tb.names.map(tk.apply) +: ts.map(_.asTableRow)

  def asSeqTable: SeqTable =
    tb.names +: ts.map(_.asTableRow)

  def renderTable(tk: TableKey): String =
    val cols = tb.fields.map(field =>
      pad(ts.map(field.plainText).map(_.toString).prepended(tk(field.name)), field.align_right))
    (0 to ts.length).map(i => cols.map(_.apply(i)).mkString("  ")).mkString("\n") + "\n"

  def renderTable: String =
    val cols = tb.fields.map(field =>
      pad(ts.map(field.plainText).map(_.toString).prepended(field.name), field.align_right))
    (0 to ts.length).map(i => cols.map(_.apply(i)).mkString("  ")).mkString("\n") + "\n"

//--- Other extensions ---

extension[T] (ts: java.util.List[java.util.List[T]])
  def asNestedSeq: Vector[Vector[String]] =
    if ts == null then Vector()
    else ts.asScala.toVector.map(_.asScala.toVector.map(_.toString))

extension[T] (ts: Seq[T])
  def multiIndex(is: Seq[Int]): Seq[T] = for i <- is yield ts(i)

extension(d: Double)
  def dpl(places: Int): String = s"%.${places}f".format(d)

extension(s: String)
  def ifEmpty(t: String): String = if s.isEmpty then t else s

//--- Helpers ---

def whitespace(n: Int, acc: String = ""): String =
  if n == 0 then acc else whitespace(n - 1, acc + " ")

def pad(xs: Seq[String], left: Boolean = false): Seq[String] =
  val max_length = xs.map(_.length).max
  if left
    then xs.map(x => whitespace(max_length - x.length) + x)
    else xs.map(x => x + whitespace(max_length - x.length))
