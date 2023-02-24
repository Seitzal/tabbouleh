package wuebutab

case class Result(context: Context, out: Output):
  def output(output: Output) = Result(context, output)
  def updateTab(tab: Tab) = Result(Context(tab, context.view), out)
  def updateView(view: View) = Result(Context(context.tab, view), out)
  def fail(reason: String) = Result(context, Output.Failure(reason))

object Result:
  def apply(): Action = context ?=> new Result(context, Output.Empty)