package wuebutab

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Success, Failure}

class CLI (arguments: Seq[String]) extends ScallopConf(arguments):

  version("Wuebutab 0.1 DEV, (c) 2024 Alex Seitz / Debating Society Germany e.V.\n")

  banner("Usage: wbt [remote|fetch|pair|alloc] [OPTION]...\n\nOptions:\n")

  object remote extends Subcommand("remote", "r"):

    val spreadsheet_id = trailArg[String]()

  object fetch extends Subcommand("fetch", "f")

  object pair extends Subcommand("pair", "p"):

    val update = toggle(default = Some(true))

    val rounds = trailArg[List[Int]](
      descr = "List of rounds (round numbers) to generate pairings for. If none are specified, will pair the next round",
      default = None,
      required = false)

  object speakers extends Subcommand("speakers", "s"):

    val minrounds = opt[Int](
      descr = "Number of rounds a speaker must have spoken in to be considered for the ranking",
      default = Some(1))

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

  object test extends Subcommand("test",  "t")

  addSubcommand(remote)
  addSubcommand(fetch)
  addSubcommand(pair)
  addSubcommand(speakers)
  addSubcommand(alloc)
  addSubcommand(test)

  verify()
  
  this.subcommand match

    case Some(_: this.remote.type) =>
      val id = this.remote.spreadsheet_id()
      Actions.setRemote(id)

    case Some(_: this.fetch.type) => 
      Actions.checkBallots()

    case Some(_: this.pair.type) =>
      val rounds = this.pair.rounds.get
      val update = this.pair.update()
      Actions.generatePairings(rounds, update)

    case Some(_: this.speakers.type) =>
      val minrounds = this.speakers.minrounds.get.getOrElse(1)
      Actions.generateSpeakerRanking(minrounds)

    case Some(_: this.alloc.type) =>
      val teams = read_teams_csv(this.alloc.teams_path())
      val judges = read_judges_csv(this.alloc.judges_path())
      val dt = if this.alloc.debate_type() == "i" then DebateType.Impromptu else DebateType.Prepared 
      val pairings = read_pairings_csv(this.alloc.pairings_path(), teams, dt)
      val panels = make_panels(pairings, judges, PanelWeights())
      println(render_table(panels))
      write_csv(panels, this.alloc.out())
      if this.alloc.update() then
        write_csv(judges.map(_.apply_panels(panels)), this.alloc.judges_path())

    case Some(_: this.test.type) =>
      Actions.test()

    case _ =>
      this.printHelp()

@main def main(args: String*): Unit = new CLI(args)
