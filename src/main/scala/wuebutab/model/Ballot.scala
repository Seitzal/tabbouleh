package wuebutab

trait JudgeStatus
object JudgeStatus:
  case object Chair extends JudgeStatus
  case object Panellist extends JudgeStatus
  case object Shadow extends JudgeStatus
  def apply(s: String): JudgeStatus = s match
    case s if s.startsWith("Chair") => Chair
    case s if s.startsWith("Panel") => Panellist
    case s if s.startsWith("Shadow") => Shadow

def checkScore(score: Double, position: String, rangeMin: Double, rangeMax: Double): Vector[String] =
  if score >= rangeMin && score <= rangeMax then Vector()
  else Vector(s"$position score of $score is outside range of $rangeMin - $rangeMax")

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

  def checkScores: Vector[String] =
    (0 to 3).toVector.map(i => checkScore(propScores(i), s"Prop ${i + 1}", if i < 3 then 60 else 30, if i < 3 then 80 else 40)).flatten ++
    (0 to 3).toVector.map(i => checkScore(oppScores(i), s"Opp ${i + 1}", if i < 3 then 60 else 30, if i < 3 then 80 else 40)).flatten

  def checkResult: Vector[String] =
    if winningSide == "Proposition" && winningTeam != prop then Vector(s"Winning team $winningTeam is not Prop, but Prop is given as winning side")
    else if winningSide == "Opposition" && winningTeam != opp then Vector(s"Winning team $winningTeam is not Opp, but Opp is given as winning side")
    else if winningSide == "Proposition" && propTotal <= oppTotal then Vector(s"Winning team $winningTeam (Prop) does not have higher score ($propTotal <= $oppTotal)")
    else if winningSide == "Opposition" && propTotal >= oppTotal then Vector(s"Winning team $winningTeam (Opp) does not have higher score ($propTotal >= $oppTotal)")
    else Vector()

  def check: Vector[String] = checkScores ++ checkResult
    
object Ballot:

  def apply(row: Vector[String]): Ballot = Ballot(
    round = row(2).toInt,
    judgeEmail = row(1), 
    judgeName = row(3), 
    judgeStatus = JudgeStatus(row(4)),
    prop = row(5),
    opp = row(6),
    propNames = Vector(7, 9, 11, 13).map(row(_)),
    propScores = 
      Vector(8, 10, 12, 14)
      .map(row(_))
      .map(_.replace(",", "."))
      .map(_.toDouble),
    oppNames = Vector(15, 17, 19, 21).map(row(_)),
    oppScores = 
      Vector(16, 18, 20, 22)
      .map(row(_))
      .map(_.replace(",", "."))
      .map(_.toDouble),
    winningTeam = row(23),
    winningSide = row(24)
  )

  def fetchAll(sheet: SpreadsheetHandle, range: String): Vector[Ballot] =
    sheet.readRange(range).tail.map(Ballot(_)).filter(_.judgeStatus != JudgeStatus.Shadow)

  def checkAll(ballots: Vector[Ballot]): Vector[String] =
    (0 until ballots.length).map(i => ballots(i).check.map(s => s"Ballot #${i + 2}: $s")).toVector.flatten
