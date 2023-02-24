package wuebutab

import com.github.tototoshi.csv._

def read_teams_csv(path: String): Vector[Team] =
  val reader = CSVReader.open(java.io.File(path))
  val teams = reader.iteratorWithHeaders.map(Team.apply).toVector
  reader.close()
  teams

def read_judges_csv(path: String): Vector[Judge] =
  val reader = CSVReader.open(java.io.File(path))
  val judges = reader.iteratorWithHeaders.map(Judge.apply).toVector
  reader.close()
  judges.filter(j => !j.name.startsWith("DIVISION") && !j.name.startsWith("TOTAL"))

def read_pairings_csv(path: String, teams: Seq[Team], dt: DebateType): Vector[Pairing] =
  val reader = CSVReader.open(java.io.File(path))
  val pairings = reader.iteratorWithHeaders.map(kv => Pairing(kv, teams, dt)).toVector
  reader.close()
  pairings

def write_csv[T](ts: Seq[T], path: String)(using tb: Tabulatable[T]): Unit =
  val writer = CSVWriter.open(java.io.File(path))
  val kvs = ts.map(tb.to_csv)
  val keys = tb.order_csv(kvs.map(_.keySet).reduce(_ union _))
  writer.writeRow(keys)
  for row <- kvs do writer.writeRow(keys.map(row.getOrElse(_, "")))
  writer.close()
