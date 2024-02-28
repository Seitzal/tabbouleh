package wuebutab

case class Round(roundNo: Int, debateType: DebateType, pairing: Option[Weights])

object Round:
  def apply(row: Vector[String]): Round = Round(
    row(0).toInt, 
    if row(1).startsWith("i") || row(1).startsWith("I") then DebateType.Impromptu else DebateType.Prepared,
    if row(2) == "manual" then None else Some(Weights(row(2), row(3).toDouble))
  )

  def fetchAll(sheet: SpreadsheetHandle, range: String): Vector[Round] =
    sheet.readRange(range).tail.map(Round(_))

