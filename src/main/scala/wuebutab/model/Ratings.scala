package wuebutab

val INITIAL_RATING = 1500
val DIVISOR = 400
val K = 32

case class PartialRatingsRow(
  date: String,
  tournament: String,
  round: String,
  prop: String,
  opp: String,
  ballots_prop: Int,
  ballots_opp: Int):

  def score_prop = ballots_prop.toDouble / (ballots_prop + ballots_opp)
  def score_opp = ballots_opp.toDouble / (ballots_prop + ballots_opp)

  // def score_prop = if ballots_prop > ballots_opp then 1d else 0d
  // def score_opp = if ballots_prop < ballots_opp then 1d else 0d

  def expected_score(ra: Int, rb: Int) = 
    1d / (1 + math.pow(10, (rb.toDouble - ra.toDouble) / DIVISOR))

  def expected_prop(ratings: Map[String, Int]) =
    expected_score(ratings.getOrElse(prop, INITIAL_RATING), ratings.getOrElse(opp, INITIAL_RATING))

  def expected_opp(ratings: Map[String, Int]) =
    expected_score(ratings.getOrElse(opp, INITIAL_RATING), ratings.getOrElse(prop, INITIAL_RATING))

  def new_rating_prop(ratings: Map[String, Int]) =
    (ratings.getOrElse(prop, INITIAL_RATING) + K * (score_prop - expected_prop(ratings))).toInt

  def new_rating_opp(ratings: Map[String, Int]) =
    (ratings.getOrElse(opp, INITIAL_RATING) + K * (score_opp - expected_opp(ratings))).toInt

  def calculate(ratings: Map[String, Int]) = RatingsRow(
    this,
    score_prop,
    score_opp,
    ratings.getOrElse(prop, INITIAL_RATING),
    ratings.getOrElse(opp, INITIAL_RATING),
    expected_prop(ratings),
    expected_opp(ratings),
    new_rating_prop(ratings),
    new_rating_opp(ratings))

case class RatingsRow(
  partial: PartialRatingsRow,
  score_prop: Double,
  score_opp: Double,
  old_rating_prop: Int,
  old_rating_opp: Int,
  expected_prop: Double,
  expected_opp: Double,
  new_rating_prop: Int,
  new_rating_opp: Int):

  def change_prop = new_rating_prop - old_rating_prop
  def change_opp = new_rating_opp - old_rating_opp

  def toVector = Vector(score_prop, score_opp, old_rating_prop, old_rating_opp, 
    expected_prop, expected_opp, new_rating_prop, new_rating_opp, change_prop,
    change_opp)

case class Ratings(
  rows: Vector[RatingsRow],
  ratings: Map[String, Int]):

  def table: Vector[Vector[String | Int]] =
    Vector("Team", "Rating") +:
    ratings
      .toVector
      .sortBy((team, rating) => rating)
      .reverse
      .map((team, rating) => Vector(team, rating))

object Ratings:

  def fetchAll(remote: SpreadsheetHandle, sheetName: String) =
    remote.readRange(s"$sheetName!A2:I").map(row => PartialRatingsRow(
      row(0), row(1), row(2), row(3), row(4), row(5).toInt, row(6).toInt
    ))

  def calculate(
      rows: Vector[PartialRatingsRow], 
      acc: Vector[RatingsRow] = Vector(),
      ratings: Map[String, Int] = Map()): 
      Ratings =
    if rows.isEmpty then Ratings(acc, ratings)
    else
      val row = rows.head.calculate(ratings)
      val newRatings = ratings
        .updated(row.partial.prop, row.new_rating_prop)
        .updated(row.partial.opp, row.new_rating_opp)
      calculate(rows.tail, acc :+ row, newRatings)
