package wuebutab

import upickle.default.*
import java.io.File

case class ConfigSheetNames(
  ballots: String,
  structure: String,
  teams: String
) derives ReadWriter

case class Config(
  sheetNames: ConfigSheetNames
) derives ReadWriter

object Config:
  val default = read[Config](File("config.json"))
