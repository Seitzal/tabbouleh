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
  
  def test(): Unit =
    val remote = getRemote
    if !remote.sheetExists("Test Output") then remote.createSheet("Test Output")
    remote.writeRange("Test Output", List(List("1", "2", "3")))
