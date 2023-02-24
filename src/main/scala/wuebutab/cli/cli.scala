package wuebutab

import org.springframework.shell.standard._
import org.springframework.boot._
import org.springframework.boot.autoconfigure.SpringBootApplication

@SpringBootApplication @ShellComponent 
class CLI:

  @ShellMethod(
    key   = Array("pair", "p"),
    value = "Generate pairings for a new round")
  def pair(
    @ShellOption(
      value        = Array("teams", "-t"),
      defaultValue = "teams.csv")
    path_teams: String,
    @ShellOption(
      value        = Array("out", "-o"),
      defaultValue = "pairings.csv")
    path_out: String,
    @ShellOption(
      value        = Array("update", "-u"),
      defaultValue = "true")
    update: Boolean
  ): Unit =
    val teams = read_teams_csv(path_teams)
    val tab = Tab("", teams, Vector())
    val pairings = make_pairings(tab.active_teams, DebateType.Impromptu, Weights.POWER_WINEXP)
    println(render_table(pairings))
    write_csv(pairings, path_out)
    if update then 
      val tab = Tab("", teams, Vector()).apply_pairings(pairings)
      write_csv(tab.teams, path_teams)
  

  // @ShellMethod(
  //   key = Array("undo", "z"),
  //   value = "undo the last n actions")
  // def undo(
  //   @ShellOption(defaultValue = "1") n: Int
  // ) =
  //   history.lift(n) match
  //     case Some(result) =>
  //       history = result :: history
  //       println(s"$n actions undone, use 'undo 1' to revert")
  //     case None => 
  //       System.err.println(s"Cannot undo $n actions as there are only ${history.length - 1} actions in history")
    
  // @ShellMethod(
  //   key = Array("view", "v"),
  //   value = "change to the given view")
  // def changeView(
  //   @ShellOption(defaultValue = ShellOption.NULL) view: String
  // ) = transact(View.change(view))

  // @ShellMethod(
  //   key = Array("show", "s"),
  //   value = "show the given view (without changing to it)")
  // def showView(
  //   @ShellOption(defaultValue = ShellOption.NULL) view: String
  // ) = transact(View.show(view))

  // @ShellMethod(
  //   key = Array("add", "a"),
  //   value = "add a new entry to the current view")
  // def add(
  //   @ShellOption(defaultValue = ShellOption.NULL, value = Array("view", "-v")) view: String,
  //   @ShellOption(defaultValue = ShellOption.NULL, value = Array("data", "-d")) data: Array[String]
  // ) = transact(View.add(view, data))

@main def main(args: String*): Unit =
  val app = SpringApplication(classOf[CLI])
  app.setBannerMode(Banner.Mode.OFF)
  app.run(args:_*)
