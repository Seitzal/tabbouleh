package wuebutab

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Success, Failure}

object Actions:

  private def getRemote: SpreadsheetHandle = 
    SpreadsheetHandle(Source.fromFile("remote").mkString) match
      case Success(sheet) => sheet
      case Failure(e) => 
        println("Error accessing remote. Please confirm that credentials and spreadsheet ID are correct.")
        throw(e)

  def setRemote(spreadsheetId: String): Unit = 
    SpreadsheetHandle(spreadsheetId) match
      case Success(sheet) => 
        val pw = PrintWriter("remote")
        pw.write(spreadsheetId)
        pw.close()
        println(s"Set remote to spreadsheet '${sheet.getTitle}' (https://docs.google.com/spreadsheets/d/$spreadsheetId)")
      case Failure(e) =>
        println("Error changing remote. Please confirm that credentials and spreadsheet ID are correct.")
        e.printStackTrace()
  
  def checkBallots(): Unit =
    val remote = getRemote
    val ballots = Ballot.fetchAll(remote, Config.default.sheetNames.ballots)
    val rounds = Round.fetchAll(remote, Config.default.sheetNames.structure)
    val problems = Ballot.checkAll(ballots)
    val meta = TeamMeta.fetchAll(remote, Config.default.sheetNames.teams)
    println(s"${ballots.length} ballots checked, ${problems.length} problems found:")
    problems.foreach(println)
    println(Results(ballots, rounds, meta))

  def generatePairings(rounds: Option[List[Int]], update: Boolean): Unit =
    val remote = getRemote
    val ballots = Ballot.fetchAll(remote, Config.default.sheetNames.ballots)
    val problems = Ballot.checkAll(ballots)
    if !problems.isEmpty then
      println("Some ballots are problematic. Please address problems before pairing, or override with '-f'.")
      println(s"${problems.length} problems found:")
      problems.foreach(println)
    else
      val structure = Round.fetchAll(remote, Config.default.sheetNames.structure)
      val meta = TeamMeta.fetchAll(remote, Config.default.sheetNames.teams)
      val results = Results(ballots, structure, meta)
      def iter(rounds_remaining: List[Int], teams: Vector[Team]): Unit =
        if !rounds_remaining.isEmpty then
          structure.find(_.roundNo == rounds_remaining.head) match
            case Some(round) =>
              val pairings = make_pairings(
                teams.filter(_.active), 
                round.debateType, 
                round.pairing.getOrElse(Weights.RANDOM),
                round.roundNo)
                .sortBy(p => (p.division, p.mean_rank_score)) 
              println(s"Pairings for round ${round.roundNo}:")
              println(render_table(pairings))
              if update then 
                write_csv(pairings, File(s"pairings${round.roundNo}.csv"))
                val sheetName = s"Round ${round.roundNo}"
                if !remote.sheetExists(sheetName) then remote.createSheet(sheetName)
                remote.writeRange(sheetName, pairings.asSeqTable.select("Div", "Prop", "Opp"))
              iter(rounds_remaining.tail, teams.map(_.apply_pairings(pairings, round.roundNo)))
            case None => 
              println(s"Couldn't generate pairings for round ${rounds_remaining.head}: Not found in structure.")
              iter(rounds_remaining.tail, teams)
        else if update then
            write_csv(teams, File("teams.csv"))
            Team.updateRemote(remote, Config.default.sheetNames.teams, teams)
      rounds match
        case Some(rs) => iter(rs, results.teams) 
        case None => iter(List(results.rounds_completed.max + 1), results.teams)
  
  def generateSpeakerRanking(minRounds: Int): Unit =
    val remote = getRemote
    val ballots = Ballot.fetchAll(remote, Config.default.sheetNames.ballots)
    val names = if remote.sheetExists("Names") then Names.fetch(remote, "Names") else Names.empty
    val speeches_old = if remote.sheetExists("Speeches") then Some(remote.readRange("Speeches")) else None
    val speeches = Speech.getAll(ballots, names, speeches_old)
    if !remote.sheetExists("Speeches") then remote.createSheet("Speeches")
    remote.writeRange("Speeches", speeches.asSeqTable)
    val newNames = names.update(speeches)
    if !remote.sheetExists("Names") then remote.createSheet("Names")
    remote.writeRange("Names", newNames.asSeqTable)
    val speakers = Speaker.getAll(newNames, speeches)
    val ranking = speakers.filter(_.roundsSpoken.length >= minRounds).sortBy(0 - _.average)
    if !remote.sheetExists("Speaker Ranking") then remote.createSheet("Speaker Ranking")
    remote.writeRange("Speaker Ranking", Vector("Rank") +: (1 to ranking.length).map(i => Vector(i.toString)))
    remote.writeRange("Speaker Ranking!B1", ranking.asSeqTable)
    println(render_table(ranking))

  def allocateJudges(round: Int, update: Boolean): Unit =
    val remote = getRemote
    val ballots = Ballot.fetchAll(remote, Config.default.sheetNames.ballots)
    val structure = Round.fetchAll(remote, Config.default.sheetNames.structure)
    val meta = TeamMeta.fetchAll(remote, Config.default.sheetNames.teams)
    val results = Results(ballots, structure, meta)
    val draws = Draw.fetchAll(remote)
    val judges = Judge.fetchAll(remote, "Judges").map(_.updateForRounds(draws.values.toSeq))
    if !draws.isDefinedAt(round) then throw Error(s"No pairings for round $round found in remote.")
    else
      val draw = draws(round)
      val pairings = for row <- draw.tail yield
        val prop = results.teams.filter(_.name == row(1)).head
        val opp = results.teams.filter(_.name == row(2)).head
        Pairing(prop, opp, DebateType.Impromptu, 0d)
      val panels = make_panels(pairings, judges.filter(_.active), PanelWeights())
      println(render_table(panels))
      if update then remote.writeRange(s"'Round $round'!E2", panels.map(_.toTableRow))

  def test(): Unit =
    val ratings = Ratings.calculate(Ratings.fetchAll(getRemote, "Match Results"))
    getRemote.writeRangeRaw("Match Results!H2", ratings.rows.map(_.toVector))
    getRemote.writeRangeRaw("Table", ratings.table)