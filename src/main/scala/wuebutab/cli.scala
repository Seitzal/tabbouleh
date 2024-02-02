package wuebutab

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Success, Failure}

class CLI (arguments: Seq[String]) extends ScallopConf(arguments):

  version("Wuebutab 0.1 DEV, (c) 2023 Alex Seitz / Debating Society Germany e.V.\n")

  banner("Usage: wbt [remote|fetch|pair|alloc] [OPTION]...\n\nOptions:\n")

  object fetch extends Subcommand("fetch", "f")

  object pair extends Subcommand("pair", "p"):

    val input_files = group("Input files:")
    val teams_path = opt[File](
      descr = "CSV file containing the team table",
      default = Some(File("teams.csv")), 
      group = input_files)
    validateFileExists(teams_path)

    val output_files = group("Output files:")
    val out = opt[File](
      descr = "Where to write the CSV file containing the generated pairings",
      default = Some(File("pairings.csv")), 
      group = output_files)
    val update = toggle(
      descrYes = "Specify whether to update the team table after generating the pairings",
      default = Some(false), 
      group = output_files)

    val engine = group("Pairing engine arguments:")
    val debate_type = choice(
      Seq("i", "p"), 
      descr = "Specify whether the debate will be (i)mpromptu or (p)repared.",
      default = Some("i"), 
      group = engine)
    val weights = choice(
      Seq("random", "power", "power_winexp", "hybrid", "hybrid_winexp"), 
      descr = "Specify a weight configuration for the engine to use. ",
      default = Some("power_winexp"), 
      group = engine)
    val random_factor = opt[Double](
      descr = "Weight of the random component of the weight configuration. Only affects hybrid or hybrid_winexp weight configs.",
      default = Some(0d), 
      group = engine)

  object alloc extends Subcommand("alloc", "a"):
    val teams_path = opt[File](default = Some(File("teams.csv")))
    val judges_path = opt[File](default = Some(File("judges.csv")))
    val pairings_path = opt[File](default = Some(File("pairings.csv")))
    val out = opt[File](default = Some(File("panels.csv")))
    val update = toggle(default = Some(false))
    val debate_type = choice(Seq("i", "p"), default = Some("i"))
    validateFileExists(teams_path)
    validateFileExists(judges_path)
    validateFileExists(pairings_path)

  object remote extends Subcommand("remote", "r"):
    val spreadsheet_id = trailArg[String]()

  addSubcommand(remote)
  addSubcommand(pair)
  addSubcommand(alloc)
  addSubcommand(fetch)

  verify()
  
  this.subcommand match

    case Some(_: this.pair.type) =>
      val teams = read_teams_csv(this.pair.teams_path())
      val dt = if this.pair.debate_type() == "i" then DebateType.Impromptu else DebateType.Prepared 
      val weights = Weights(this.pair.weights(), this.pair.random_factor())
      val pairings = make_pairings(teams.filter(_.active), dt, weights)
        .sortBy(_.mean_rank_score).reverse.sortBy(_.division)
      println(render_table(pairings))
      write_csv(pairings, this.pair.out())
      if this.pair.update() then
        write_csv(teams.map(_.apply_pairings(pairings)), this.pair.teams_path())

    case Some(_: this.alloc.type) =>
      val teams = read_teams_csv(this.alloc.teams_path())
      val judges = read_judges_csv(this.alloc.judges_path())
      val dt = if this.pair.debate_type() == "i" then DebateType.Impromptu else DebateType.Prepared 
      val pairings = read_pairings_csv(this.alloc.pairings_path(), teams, dt)
      val panels = make_panels(pairings, judges, PanelWeights())
      println(render_table(panels))
      write_csv(panels, this.alloc.out())
      if this.alloc.update() then
        write_csv(judges.map(_.apply_panels(panels)), this.alloc.judges_path())

    case Some(_: this.remote.type) =>
      val id = this.remote.spreadsheet_id()
      SpreadsheetHandle(id) match
        case Success(sheet) => 
          val pw = PrintWriter("remote")
          pw.write(id)
          pw.close()
          println(s"Set remote to spreadsheet '${sheet.getTitle}' (https://docs.google.com/spreadsheets/d/$id)")
        case Failure(e) =>
          println("Error changing remote:")
          e.printStackTrace()
          println("Please confirm that credentials and spreadsheet ID are correct.")

    case Some(_: this.fetch.type) => withRemote(sheet =>
      val ballots = Ballot.fetchAll(sheet, "Formularantworten 1")
      val rounds = Round.fetchAll(sheet, "Structure")
      val problems = Ballot.checkAll(ballots)
      println(s"${ballots.length} ballots checked, ${problems.length} problems found:")
      problems.foreach(println)
      println(Results(ballots, rounds))
    )
      
    case _ =>
      this.printHelp()

  def withRemote(f: SpreadsheetHandle => Unit) = 
    SpreadsheetHandle(Source.fromFile("remote").mkString) match
      case Success(sheet) => f(sheet)
      case Failure(e) => 
        println("Error accessing remote:")
        e.printStackTrace()
        println("Please confirm that credentials and spreadsheet ID are correct.")

@main def main(args: String*): Unit = new CLI(args)
