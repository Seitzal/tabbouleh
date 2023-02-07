package wuebutab

def whitespace(n: Int, acc: String = ""): String =
  if n == 0 then acc else whitespace(n - 1, acc + " ")

def pad(xs: Seq[String]): Seq[String] =
  val max_length = xs.map(_.length).max
  xs.map(x => whitespace(max_length - x.length) + x)

trait Tabulatable[T]:
  def fields: Seq[(String, T => Any)]

def print_table[T](ts: Seq[T])(using tb: Tabulatable[T]): Unit =
  val fields_padded = tb.fields.map {
    case (key, f) => pad(ts.map(f).map(_.toString()).prepended(key))
  }
  for i <- 0 to ts.length do
    for f <- fields_padded do
      print(f(i) + "  ")
    println()

given Tabulatable[Team] = new Tabulatable[Team]:
  def fields: Vector[(String, Team => Any)] = Vector(
    "ID" -> (_.id),
    "Team" -> (_.name)
  )

given Tabulatable[Pairing] = new Tabulatable[Pairing]:
  def fields: Vector[(String, Pairing => Any)] = Vector(
    "MRS"         -> (p => 
      val mrs = p.mean_rank_score
      f"$mrs%.8f"
    ),
    "Proposition" -> (_.prop.name),
    "W"        -> (_.prop.wins),
    "B"     -> (_.prop.ballots),
    "P"      -> (p => 
      val points = p.prop.points
      f"$points%.2f"
    ),
    "SP"  -> (p => p.prop.side_pref(p.dt)),
    "Ooposition"  -> (_.opp.name),
    "W"        -> (_.opp.wins),
    "B"     -> (_.opp.ballots),
    "P"      -> (p => 
      val points = p.opp.points
      f"$points%.2f"
    ),
    "SP"  -> (p => p.opp.side_pref(p.dt)),
    "Rematch"     -> (_.rematch),
    "Weight"      -> (p => 
      val weight = p.weight
      f"$weight%.6f"
    ),
  )