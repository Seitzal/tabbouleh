package wuebutab

case class Context(tab: Tab, view: View)

type Action = Context ?=> Result

extension(a: Action)
  def *>(b: Action)(using context: Context): Result =
    val res_a = a.apply(using context)
    val res_b = b.apply(using res_a.context)
    res_b.output(Output.Compound(res_a.out, res_b.out))

extension(as: Seq[Action])
  def sequence(acc: Action = Result()): Action =
    as.headOption.match
      case Some(a1) => as.tail.sequence(acc *> a1)
      case None => acc

extension[T](xs: Seq[T])
  def seqAction(f: T => Action): Action = xs.map(f).sequence()
