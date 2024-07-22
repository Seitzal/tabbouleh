package wuebutab

import scala.util.Random
import monocle._
import monocle.syntax.all._
import cats.implicits._

trait JudgeStatus

object JudgeStatus:
  case object Chair extends JudgeStatus
  case object Panellist extends JudgeStatus
  case object Shadow extends JudgeStatus
  def apply(s: String): JudgeStatus = s match
    case s if s.startsWith("Chair") => Chair
    case s if s.startsWith("Panel") => Panellist
    case s if s.startsWith("Shadow") => Shadow

enum Side(val symbol: String):
  case Proposition extends Side("P")
  case Opposition extends Side("O")

object Side:
  def fromSymbol(symbol: String) =
    if symbol.equalsIgnoreCase("P") then Some(Proposition) 
    else if symbol.equalsIgnoreCase("O") then Some(Opposition)
    else None

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
    case (DebateType.Prepared, Side.Proposition) => SidePref(overall + 1, prep + 1, impr)
    case (DebateType.Prepared, Side.Opposition) => SidePref(overall - 1, prep - 1, impr)
    case (DebateType.Impromptu, Side.Proposition) => SidePref(overall + 1, prep, impr + 1)
    case (DebateType.Impromptu, Side.Opposition) => SidePref(overall - 1, prep, impr - 1)

