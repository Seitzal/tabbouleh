package wuebutab

sealed trait View:
  def prompt: String
  def parent: View
  def show(using context: Context): Output
  def add(args: Array[String])(using context: Context) : Result

object View:

  def apply(viewName: String)(using context: Context): Option[View] =
    viewName match
      case null => Some(context.view)
      case "." => Some(context.view)
      case ".." => Some(context.view.parent)
      case "global" => Some(Global)
      case "teams" => Some(Teams)
      case _ => None

  def change(viewName: String)(using context: Context): Result =
    View(viewName) match
      case Some(view) => Result().updateView(view).output(view.show)
      case None => Result().fail(s"Unknown view '$viewName'")

  def show(viewName: String)(using context: Context): Result =
    View(viewName) match
      case Some(view) => Result().output(view.show)
      case None => Result().fail(s"Unknown view '$viewName'")

  def add(viewName: String, data: Array[String])(using context: Context): Result =
    View(viewName) match
      case Some(view) => view.add(data)
      case None => Result().fail(s"Unknown view '$viewName'")

  object Global extends View:
    def prompt = "global"
    def parent = this // This is the uppermost view
    def show(using context: Context) =
      context.tab.printSummary
    def add(data: Array[String])(using context: Context) : Result =
      Result().fail(s"Global view does not support command 'add'")

  object Teams extends View:
    def prompt = "teams"
    def parent = Global
    def show(using context: Context) = context.tab.printTeams
    def add(data: Array[String])(using context: Context): Result =
      data.seqAction(Tab.addTeam)
