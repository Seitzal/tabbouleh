package wuebutab

import org.springframework.shell.standard._
import org.springframework.boot._
import org.springframework.boot.autoconfigure.SpringBootApplication

import wuebutab.View
@SpringBootApplication @ShellComponent 
class Actions:

  private var history = List(Result(Context(Tab(), View.Global), Output.Empty))

  private def transact(action: Action): Unit =
    given Context = history.head.context
    val result = action
    history = result :: history
    result.out.show()

  @ShellMethod(
    key = Array("undo", "z"),
    value = "undo the last n actions")
  def undo(
    @ShellOption(defaultValue = "1") n: Int
  ) =
    history.lift(n) match
      case Some(result) =>
        history = result :: history
        println(s"$n actions undone, use 'undo 1' to revert")
      case None => 
        System.err.println(s"Cannot undo $n actions as there are only ${history.length - 1} actions in history")
    
  @ShellMethod(
    key = Array("view", "v"),
    value = "change to the given view")
  def changeView(
    @ShellOption(defaultValue = ShellOption.NULL) view: String
  ) = transact(View.change(view))

  @ShellMethod(
    key = Array("show", "s"),
    value = "show the given view (without changing to it)")
  def showView(
    @ShellOption(defaultValue = ShellOption.NULL) view: String
  ) = transact(View.show(view))

  @ShellMethod(
    key = Array("add", "a"),
    value = "add a new entry to the current view")
  def add(
    @ShellOption(defaultValue = ShellOption.NULL, value = Array("view", "-v")) view: String,
    @ShellOption(defaultValue = ShellOption.NULL, value = Array("data", "-d")) data: Array[String]
  ) = transact(View.add(view, data))

def main(args: String*): Unit =
  val app = SpringApplication(classOf[Actions])
  app.setBannerMode(Banner.Mode.OFF)
  app.run(args:_*)
