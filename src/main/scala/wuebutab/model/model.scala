package wuebutab

import scala.util.Random
import monocle._
import monocle.syntax.all._
import cats.implicits._
import simulacrum.op

enum Side(val symbol: String):
  case Proposition extends Side("P")
  case Opposition extends Side("O")

enum DebateType(val symbol: String):
  case Impromptu extends DebateType("i")
  case Prepared extends DebateType("p")

case class SidePref(
  overall: Int = 0,
  prep: Int = 0,
  impr: Int = 0):
  def apply(dt: DebateType): Int =
    if dt == DebateType.Prepared && prep != 0 then prep
    else if dt == DebateType.Impromptu && impr != 0 then impr
    else overall
  override def toString(): String = s"$overall $prep $impr"
  def apply_pairing(p: Pairing, s: Side): SidePref = (p.dt, s) match   
    case (DebateType.Prepared, Side.Proposition) => SidePref(overall + 2, prep + 2, impr)
    case (DebateType.Prepared, Side.Opposition) => SidePref(overall - 2, prep - 2, impr)
    case (DebateType.Impromptu, Side.Proposition) => SidePref(overall + 2, prep, impr + 2)
    case (DebateType.Impromptu, Side.Opposition) => SidePref(overall - 2, prep, impr - 2)

