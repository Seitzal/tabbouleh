package wuebutab

trait Output:
  def show(): Unit

object Output:

  def apply(f: () => Unit) = new Output:
    def show() = f()

  object Empty extends Output:
    def show() = {}

  final case class Simple(text: String) extends Output:
    def show() = println(text)

  final case class Failure(text: String) extends Output:
    def show() = System.err.println(scala.io.AnsiColor.RED + text)

  final case class Compound(o1: Output, o2: Output) extends Output:
    def show() = 
      o1.show()
      o2.show()
