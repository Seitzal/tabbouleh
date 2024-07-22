package wuebutab

object Draw:

  def fetchAll(remote: SpreadsheetHandle): Map[Int, SeqTable] =
    val sheetNames = remote.sheetNames.filter(_.startsWith("Round"))
    val roundNumbers = sheetNames.map(_.drop(6).parseInt)
    val tables = sheetNames.map(remote.readRange).map(_.padRight)
    roundNumbers.zip(tables).toMap
