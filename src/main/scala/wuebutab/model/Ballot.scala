package wuebutab

case class Ballot(
  round: Int,
  judgeEmail: String,
  judgeName: String,
  judgeStatus: JudgeStatus,
  prop: String,
  opp: String,
  propNames: Vector[String],
  propScores: Vector[Double],
  oppNames: Vector[String],
  oppScores: Vector[Double],
  winningTeam: String,
  winningSide: String):

  def propTotal = propScores.sum
  def oppTotal = oppScores.sum

  def checkScore(
      score: Double, 
      position: String, 
      rangeMin: Double, 
      rangeMax: Double): Vector[String] =
    if score >= rangeMin && score <= rangeMax then Vector()
    else Vector(s"$position score of $score is outside range of $rangeMin - $rangeMax")

  def checkScores: Vector[String] =
    (0 to 3).toVector.map(i => checkScore(
      propScores(i), 
      s"Prop ${i + 1}", 
      if i < 3 then 60 else 30, 
      if i < 3 then 80 else 40)
    ).flatten ++
    (0 to 3).toVector.map(i => checkScore(
      oppScores(i), 
      s"Opp ${i + 1}", 
      if i < 3 then 60 else 30, 
      if i < 3 then 80 else 40)
    ).flatten

  def checkResult: Vector[String] =
    if winningSide == "Proposition" && winningTeam != prop 
      then Vector(s"Winning team $winningTeam is not Prop, but Prop is given as winning side")
    else if winningSide == "Opposition" && winningTeam != opp 
      then Vector(s"Winning team $winningTeam is not Opp, but Opp is given as winning side")
    else if winningSide == "Proposition" && propTotal <= oppTotal 
      then Vector(s"Winning team $winningTeam (Prop) does not have higher score ($propTotal <= $oppTotal)")
    else if winningSide == "Opposition" && propTotal >= oppTotal 
      then Vector(s"Winning team $winningTeam (Opp) does not have higher score ($propTotal >= $oppTotal)")
    else Vector()

  def check: Vector[String] = checkScores ++ checkResult
    
object Ballot:

  def apply(row: Vector[String], header: TableHeader)(using config: Config): Ballot = 
    val key = config.tableKeys.ballots
      Ballot(
      row(header.findLocalized("round", key)).parseInt,
      row(header.findLocalized("judgeEmail", key)),
      row(header.findLocalized("judgeName", key)),
      JudgeStatus(row(header.findLocalized("judgeStatus", key))),
      row(header.findLocalized("prop", key)),
      row(header.findLocalized("opp", key)),
      Vector(
        row(header.findLocalized("propNames1", key)), 
        row(header.findLocalized("propNames2", key)), 
        row(header.findLocalized("propNames3", key)),
        row(header.findLocalized("propNames4", key))),
      Vector(
        row(header.findLocalized("propScores1", key)).toDouble, 
        row(header.findLocalized("propScores2", key)).toDouble, 
        row(header.findLocalized("propScores3", key)).toDouble,
        row(header.findLocalized("propScores4", key)).toDouble),
      Vector(
        row(header.findLocalized("oppNames1", key)), 
        row(header.findLocalized("oppNames2", key)), 
        row(header.findLocalized("oppNames3", key)),
        row(header.findLocalized("oppNames4", key))),
      Vector(
        row(header.findLocalized("oppScores1", key)).toDouble, 
        row(header.findLocalized("oppScores2", key)).toDouble, 
        row(header.findLocalized("oppScores3", key)).toDouble,
        row(header.findLocalized("oppScores4", key)).toDouble),
      row(header.findLocalized("winningTeam", key)),
      row(header.findLocalized("winningSide", key))
    )

  def fetchAll(using remote: SpreadsheetHandle, config: Config): Vector[Ballot] =
    val table = remote.readRange(config.sheetNames.ballots)
    table
      .tail
      .map(row => Ballot(row, TableHeader(table.head)))
      .filter(_.judgeStatus != JudgeStatus.Shadow)

  def checkAll(ballots: Vector[Ballot]): Vector[String] =
    (0 until ballots.length)
      .map(i => ballots(i).check.map(s => s"Ballot #${i + 2}: $s"))
      .toVector
      .flatten
