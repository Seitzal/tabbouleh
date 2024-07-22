package wuebutab

import upickle.default.*
import java.io.File

case class ConfigSheetNames(
  ballots: String,
  structure: String,
  teams: String
) derives ReadWriter

case class TableKeyColumn(
  localizedTitle: String,
  defaultPosition: Int
) derives ReadWriter

type TableKey = Map[String, TableKeyColumn]

case class ConfigTableKeys(
  ballots: TableKey
) derives ReadWriter

case class Config(
  sheetNames: ConfigSheetNames,
  tableKeys: ConfigTableKeys
) derives ReadWriter

object Config:
  val default = read[Config](File("config.json"))
