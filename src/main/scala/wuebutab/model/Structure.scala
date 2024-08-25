package wuebutab

case class Round(roundNo: Int, debateType: DebateType, pairing: Option[Weights])

object Round:
  def apply(row: Vector[String], header: TableHeader)(using config: Config): Round = 
    val key = config.tableKeys.rounds
    val weights = row(header.findLocalized("weights", key))
    Round(
      row(header.findLocalized("number", key)).parseInt,
      DebateType.fromSymbol(row(header.findLocalized("debate_type", key))),
      if weights == "manual" then None else Some(Weights(weights, row(header.findLocalized("weight_random", key)).toDouble))
    )

  def fetchAll(using remote: SpreadsheetHandle, config: Config): Vector[Round] =
    val table = remote.readRange(config.sheetNames.rounds)
    table.tail.map(row => Round(row, TableHeader(table.head)))
