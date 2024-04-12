package wuebutab

val INITIAL_RATING = 1500d
val DIVISOR = 400
val K = 64

case class RatingsRow(
  date: String,
  tournament: String,
  round: String,
  prop: String,
  opp: String,
  ballots_prop: Int,
  ballots_opp: Int):

  // def score_prop = ballots_prop.toDouble / (ballots_prop + ballots_opp)
  // def score_opp = ballots_opp.toDouble / (ballots_prop + ballots_opp)

  def score_prop = if ballots_prop > ballots_opp then 1d else 0d
  def score_opp = if ballots_prop < ballots_opp then 1d else 0d

  def expected_prop(ratings: Map[String, Double]) =
    val rating_prop = ratings.getOrElse(prop, INITIAL_RATING)
    val rating_opp = ratings.getOrElse(opp, INITIAL_RATING)
    1d / (1 + math.pow(10, (rating_opp - rating_prop) / DIVISOR))

  def expected_opp(ratings: Map[String, Double]) =
    val rating_prop = ratings.getOrElse(prop, INITIAL_RATING)
    val rating_opp = ratings.getOrElse(opp, INITIAL_RATING)
    1d / (1 + math.pow(10, (rating_prop - rating_opp) / DIVISOR))

  def new_rating_prop(ratings: Map[String, Double]) =
    ratings.getOrElse(prop, INITIAL_RATING) + K * (score_prop - expected_prop(ratings))

  def new_rating_opp(ratings: Map[String, Double]) =
    ratings.getOrElse(opp, INITIAL_RATING) + K * (score_opp - expected_opp(ratings))

  def out_row(ratings: Map[String, Double]) = Vector(
    round,
    prop,
    opp,
    ballots_prop,
    ballots_opp,
    score_prop,
    score_opp,
    ratings.getOrElse(prop, INITIAL_RATING),
    ratings.getOrElse(opp, INITIAL_RATING),
    expected_prop(ratings),
    expected_opp(ratings),
    new_rating_prop(ratings),
    new_rating_opp(ratings))

object Ratings:

  def fetchAll(remote: SpreadsheetHandle, sheetName: String) =
    remote.readRange(s"$sheetName!A2:I").map(row => RatingsRow(
      row(0), row(1), row(2), row(3), row(4), row(5).toInt, row(6).toInt
    ))

  def calculate(
      rows: Vector[RatingsRow], 
      ratings: Map[String, Double] = Map(), 
      acc: Vector[Vector[String | Int | Double]] = Vector()): 
      (Map[String, Double], Vector[Vector[String | Int | Double]]) =
    if rows.isEmpty then (ratings, acc)
    else
      val nextRow = rows.head.out_row(ratings)
      val new_rating_prop = rows.head.new_rating_prop(ratings)
      val new_rating_opp = rows.head.new_rating_opp(ratings)
      val newRatings = ratings
        .updated(rows.head.prop, new_rating_prop)
        .updated(rows.head.opp, new_rating_opp)
      calculate(rows.tail, newRatings, acc :+ nextRow)
      

