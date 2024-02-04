package wuebutab

case class TeamMeta(
  division: String,
  active: Boolean,
  sidelock: Map[Int, Side])

object TeamMeta:

  def apply(tableFields: Map[String, String]) =
    new TeamMeta(
      tableFields.getOrElse("Division", "univ."),
      tableFields.getOrElse("Active", "1") != "0",
      tableFields
        .filter((k, v) => k.startsWith("Side "))
        .map((k, v) => k.drop(5).toInt -> Side.fromSymbol(v))
    )

  def getAll(table: Vector[Vector[String]]) =
    val nameColumn = table.head.indexOf("Team")
    (for row <- table.tail yield row(nameColumn) -> TeamMeta(table.head.zip(row).toMap)).toMap

  def fetchAll(sheet: SpreadsheetHandle, range: String = "Team Meta") =
    getAll(sheet.readRange(range))
