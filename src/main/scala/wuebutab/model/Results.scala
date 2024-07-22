package wuebutab

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

class Results(ballots: Vector[Ballot], rounds: Vector[Round], meta: Map[String, TeamMeta]):

  val debateResults = ballots
    .filter(_.judgeStatus != JudgeStatus.Shadow)
    .groupBy(b => (b.round, b.prop, b.opp))
    .map((k, v) => DebateResults(v))
    .toVector
    .sortBy(_.round)

  val teams = 
    Team.getAll(debateResults, rounds, meta)
    .sortBy(_.rank_score)
    .reverse

  val rounds_completed = debateResults.map(_.round).distinct

  override def toString(): String =
    s"Results of ${debateResults.length} debates:\n" + render_table(debateResults) + "\n" +
    s"Results for ${teams.length} teams after ${rounds_completed.length} rounds:\n" + render_table(teams)
