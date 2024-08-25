package wuebutab

import upickle.default.*
import java.io.File

case class ConfigSheetNames(
  ballots: String,
  rounds: String,
  teams: String,
  judges: String
) derives ReadWriter

case class ConfigTableKeys(
  ballots: TableKey,
  teams: TableKey,
  judges: TableKey,
  rounds: TableKey
) derives ReadWriter

case class Config(
  sheetNames: ConfigSheetNames,
  tableKeys: ConfigTableKeys
) derives ReadWriter

object Config:
  val default = read[Config](File("config.json"))
