package wuebutab

import com.github.tototoshi.csv._
import java.io.File

def read_teams_csv(file: File): Vector[Team] =
  val reader = CSVReader.open(file)
  val teams = reader.iteratorWithHeaders.map(Team.apply).toVector
  reader.close()
  teams

def read_judges_csv(file: File): Vector[Judge] =
  val reader = CSVReader.open(file)
  val judges = reader.iteratorWithHeaders.map(Judge.apply).toVector
  reader.close()
  judges.filter(j => !j.name.startsWith("DIVISION") && !j.name.startsWith("TOTAL"))

def read_pairings_csv(file: File, teams: Seq[Team], dt: DebateType): Vector[Pairing] =
  val reader = CSVReader.open(file)
  val pairings = reader.iteratorWithHeaders.map(kv => Pairing(kv, teams, dt)).toVector
  reader.close()
  pairings

def write_csv[T](ts: Seq[T], file: File)(using tb: Tabulatable[T]): Unit =
  val writer = CSVWriter.open(file)
  if !ts.isEmpty then
    val kvs = ts.map(tb.to_csv)
    val keys = tb.order_csv(kvs.map(_.keySet).reduce(_ union _))
    writer.writeRow(keys)
    for row <- kvs do writer.writeRow(keys.map(row.getOrElse(_, "")))
  writer.close()
