package wuebutab

case class TeamResults(
  team: String,
  wins: Int,
  ballots: Int,
  points: Double,
  side_pref: SidePref):

  def rank_score: Double = wins + ballots * 1E-2 + points * 1E-6

object TeamResults:

  def apply(team: String, debateResults: Vector[DebateResults], rounds: Vector[Round]) =

    val regularWins = debateResults.filter(_.winner == team).length
    val regularBallots = debateResults.map(_.ballots(team)).sum
    val regularPoints = debateResults.map(_.points(team)).sum
    val regularN = debateResults.filter(_.regularMatchFor(team)).length

    val mixedN = debateResults.filter(_.mixedMatchFor(team)).length
    val naSwingN = debateResults.filter(_.naSwingFor(team)).length

    val imputedWins = mixedN + Math.round((regularWins.toFloat / regularN) * naSwingN)
    val mixedBallots = mixedN * (if regularBallots >= regularN * (3d / 2) then 3 else 2)
    val naSwingNBallots = (regularBallots.toFloat / regularN).toInt
    val imputedBallots = mixedBallots + naSwingNBallots
    val imputedPoints = (mixedN + naSwingN) * (regularPoints / regularN)

    val prepRounds = rounds.filter(_.debateType == DebateType.Prepared).map(_.roundNo)
    val imprRounds = rounds.filter(_.debateType == DebateType.Impromptu).map(_.roundNo)
    val propImprN = debateResults.filter(_.prop == team).map(_.round).filter(imprRounds.contains).length
    val oppImprN = debateResults.filter(_.opp == team).map(_.round).filter(imprRounds.contains).length
    val propPrepN = debateResults.filter(_.prop == team).map(_.round).filter(prepRounds.contains).length
    val oppPrepN = debateResults.filter(_.opp == team).map(_.round).filter(prepRounds.contains).length

    val sidePref = SidePref(
      2 * (propPrepN + propImprN) - 2 * (oppPrepN + oppImprN),
      2 * propPrepN - 2 * oppPrepN,
      2 * propImprN - 2 * oppImprN)

    new TeamResults(
      team,
      regularWins + imputedWins,
      regularBallots + imputedBallots,
      regularPoints + imputedPoints,
      sidePref)
  
  def getAll(debateResults: Vector[DebateResults], rounds: Vector[Round]) =
    for team <- (debateResults.map(_.winner) ++ debateResults.map(_.loser)).distinct 
    yield TeamResults(team, debateResults, rounds) 

  given t: Tabulatable[TeamResults] = new Tabulatable:

    def fields = Seq(
      TableField("Team", _.team, false),
      TableField("Wins", _.wins.toString, true),
      TableField("Ballots", _.ballots.toString, true),
      TableField("Points", _.points.dpl(2), true),
      TableField("SP", _.side_pref.overall.toString, true),
      TableField("P", _.side_pref.prep.toString, true),
      TableField("I", _.side_pref.impr.toString, true))

    def to_csv(tr: TeamResults): Map[String, String] = Map(
      "Team" -> tr.team,
      "Wins" -> tr.wins.toString,
      "Ballots" -> tr.ballots.toString,
      "Points" -> tr.points.toString,
      "SidePref" -> tr.side_pref.overall.toString,
      "SidePrefPrep" -> tr.side_pref.prep.toString,
      "SidePrefImpr" -> tr.side_pref.impr.toString)

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Team", "Wins", "Ballots", "Points", "SidePref", "SidePrefPrep", "SidePrefImpr")

case class DebateResults(
  round: Int,
  winner: String,
  loser: String,
  winningSide: String,
  ballotsProp: Int,
  ballotsOpp: Int,
  pointsProp: Double,
  pointsOpp: Double):

  def concurring = if winningSide == "Proposition" then ballotsProp else ballotsOpp
  def dissenting = if winningSide == "Proposition" then ballotsOpp else ballotsProp

  def prop = if winningSide == "Proposition" then winner else loser
  def opp = if winningSide == "Opposition" then winner else loser

  def ballots(team: String) = 
    if prop == team && isRegularTeam(opp) then ballotsProp 
    else if opp == team && isRegularTeam(prop) then ballotsOpp
    else 0

  def points(team: String) = 
    if prop == team && isRegularTeam(opp) then pointsProp 
    else if opp == team && isRegularTeam(prop) then pointsOpp
    else 0d

  private def isRegularTeam(team: String) =
    !team.startsWith("Mixed-Team") &&
    team != "Swing-Team (NA)"

  def regularMatchFor(team: String) =
    (prop == team && isRegularTeam(opp)) ||
    (opp == team && isRegularTeam(prop))

  def mixedMatchFor(team: String) =
    (prop == team && opp.startsWith("Mixed-Team")) ||
    (opp == team && prop.startsWith("Mixed-Team"))

  def naSwingFor(team: String) =
    (prop == team && opp == "Swing-Team (NA)") ||
    (opp == team && prop == "Swing-Team (NA)")

object DebateResults:

  def apply(ballots: Vector[Ballot]): DebateResults = 
    val ballotsProp = ballots.length match
      case 1 => if ballots(0).winningSide == "Proposition" then 3 else 0
      case 2 =>
        ballots.filter(b => b.judgeStatus == JudgeStatus.Chair && b.winningSide == "Proposition").length * 2
        + ballots.filter(b => b.judgeStatus == JudgeStatus.Panellist && b.winningSide == "Proposition").length
      case 3 =>
        ballots.filter(_.winningSide == "Proposition").length
    val ballotsOpp = 3 - ballotsProp
    val winningSide = if ballotsProp >= 2 then "Proposition" else "Opposition"
    val winner = if winningSide == "Proposition" then ballots(0).prop else ballots(0).opp
    val loser = if winningSide == "Proposition" then ballots(0).opp else ballots(0).prop
    val pointsProp = ballots.map(_.propTotal).sum * (3d / ballots.length)
    val pointsOpp = ballots.map(_.oppTotal).sum * (3d / ballots.length)
    DebateResults(ballots(0).round, winner, loser, winningSide, ballotsProp, ballotsOpp, pointsProp, pointsOpp)

  given t: Tabulatable[DebateResults] = new Tabulatable:

    def fields = Seq(
      TableField("Round", _.round, true),
      TableField("Prop", _.prop, true),
      TableField("P", _.pointsProp.dpl(2), true),
      TableField("B", _.ballotsProp, true),
      TableField("B", _.ballotsOpp, true),
      TableField("P", _.pointsOpp.dpl(2), true),
      TableField("Opp", _.opp, false))

    def to_csv(dr: DebateResults): Map[String, String] = Map(
      "Round" -> dr.round.toString,
      "Proposition Team" -> dr.prop,
      "Opposition Team" -> dr.opp,
      "Winning Team" -> dr.winner,
      "Winning Side" -> dr.winningSide,
      "Proposition Ballots" -> dr.ballotsProp.toString,
      "Opposition Ballots" -> dr.ballotsOpp.toString,
      "Proposition Points" -> dr.pointsProp.toString,
      "Opposition Points" -> dr.pointsOpp.toString)

    def order_csv(keys: Set[String]): Seq[String] =
      Vector("Round", "Proposition Team", "Opposition Team", "Winning Team", "Winning Side", "Proposition Ballots", 
        "Opposition Ballots", "Proposition Points", "Opposition Points")

class Results(ballots: Vector[Ballot], rounds: Vector[Round]):

  val debateResults = ballots
    .filter(_.judgeStatus != JudgeStatus.Shadow)
    .groupBy(b => (b.round, b.prop, b.opp))
    .map((k, v) => DebateResults(v))
    .toVector
    .sortBy(_.round)

  val teamResults = 
    TeamResults.getAll(debateResults, rounds)
    .sortBy(_.rank_score)
    .reverse

  val rounds_completed = debateResults.map(_.round).distinct
  val teams = teamResults.map(_.team).distinct

  override def toString(): String =
    s"Results of ${debateResults.length} debates:\n" + render_table(debateResults) + "\n" +
    s"Results for ${teams.length} teams after ${rounds_completed.length} rounds:\n" + render_table(teamResults)
