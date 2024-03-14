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
    
    val update = toggle(default = Some(true))

    val round = trailArg[Int](
      required = true)

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
      val round = this.alloc.round()
      val update = this.alloc.update()
      Actions.allocateJudges(round, update)

    case Some(_: this.test.type) =>
      Actions.test()

    case _ =>
      this.printHelp()

@main def main(args: String*): Unit = new CLI(args)
