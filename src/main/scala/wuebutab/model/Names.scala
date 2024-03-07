package wuebutab

case class Name(
  team: String,
  name: String
)

case class Names(harmonized: Map[Name, String]):

  def update(speeches: Vector[Speech]): Names =
    val distinct_names = speeches.map(speech => Name(speech.homeTeam, speech.name)).distinct.toList
    def integrate(names: List[Name], harmonized: Map[Name, String]): Map[Name, String] =
      if names.isEmpty then harmonized
      else if harmonized.isDefinedAt(names.head) then integrate(names.tail, harmonized)
      else integrate(names.tail, harmonized + (names.head -> names.head.name))
    Names(integrate(distinct_names, harmonized))

  def asSeqTable: SeqTable =
    val headers = Vector("Home Team", "Name", "Harmonized Name")
    val rows = for (k, v) <- harmonized.toVector yield Vector(k.team, k.name, v)
    headers +: rows.sortBy(row => (row(0), row(1)))

object Names:

  def empty = Names(Map())

  def apply(t: SeqTable): Names =
    val entries = for row <- t.tail yield Name(row(0), row(1)) -> row(2)
    Names(entries.distinct.toMap)

  def fetch(sheet: SpreadsheetHandle, range: String): Names =
    Names(sheet.readRange(range))
