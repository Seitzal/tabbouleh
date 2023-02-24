package wuebutab

import org.jgrapht.graph._
import org.jgrapht.alg.matching.blossom.v5.KolmogorovWeightedPerfectMatching

import scala.jdk.CollectionConverters._
import scala.util.Random

def side_imbalance(t1: Team, t2: Team, dt: DebateType) =
  if (t1.side_pref(dt) > 0 && t2.side_pref(dt) > 0) || (t1.side_pref(dt) < 0 && t2.side_pref(dt) < 0) 
  then math.abs(t1.side_pref(dt) + t2.side_pref(dt))
  else 0

case class Weights(
  rematch: Double,
  side_pref: Double,
  side_nofix: Double,
  repeat_pullup: Double,
  wins: Double,
  wins_expo: Boolean,
  ballots: Double,
  points: Double,
  random: Double):
  def apply_rematch(t1: Team, t2: Team) = 
    if t1.previous_opponents.contains(t2.name) then rematch else 0
  def apply_side_pref(t1: Team, t2: Team, dt: DebateType) =
    side_imbalance(t1, t2, dt) * side_pref 
  def apply_side_nofix(t1: Team, t2: Team, dt: DebateType) =
    if t1.side_pref(dt) == 0 && t2.side_pref(dt) == 0 then side_nofix else 0
  def apply_repeat_pullup(t1: Team, t2: Team) =
    if t1.wins < t2.wins then t1.pull_ups * repeat_pullup
    else if t2.wins > t1.wins then t2.pull_ups * repeat_pullup
    else 0 
  def apply_wins(t1: Team, t2: Team) =
    if wins_expo && t1.wins != t2.wins then wins * math.pow(10, math.abs(t1.wins - t2.wins))
    else wins * math.abs(t1.wins - t2.wins)
  def apply_ballots(t1: Team, t2: Team) =
    math.abs(t1.ballots - t2.ballots) * ballots
  def apply_points(t1: Team, t2: Team) =
    math.abs(t1.points - t2.points) * points
  def apply_random = 
    Random.nextDouble * random
  def apply(t1: Team, t2: Team, dt: DebateType): Double =
    apply_rematch(t1, t2) +
    apply_side_pref(t1, t2, dt) +
    apply_side_nofix(t1, t2, dt) +
    apply_repeat_pullup(t1, t2) +
    apply_wins(t1, t2) +
    apply_ballots(t1, t2) +
    apply_points(t1, t2) +
    apply_random

object Weights:
  /** Simple random pairing with a small incentive for fixing side imbalance */
  lazy val RANDOM = Weights(1000, 10, 1, 0, 0, false, 0, 0, 1)
  /** Deterministic power pairing with linear scaling for all levels of power */
  lazy val POWER = Weights(1E+9, 1E+6, 0, 3E+2, 1E+2, false, 1E+0, 1E-4, 0)
  /** Deterministic power pairing with exponential scaling for wins and linear scaling for ballots and points */
  lazy val POWER_WINEXP = Weights(1E+9, 1E+6, 0, 1E+3, 1E+1, true, 1E+0, 1E-4, 0)
  /** Power pairing with linear scaling for all levels of power and a scalable random factor */
  def HYBRID(random: Double = 1E-2) = Weights(1E+9, 1E+6, 0, 3E+2, 1E+2, false, 1E+0, 1E-4, random)
  /** Power pairing with exponential scaling for wins, linear scaling for ballots and points, and a scalable random factor */
  def HYBRID_WINEXP(random: Double = 1E-2) = Weights(1E+9, 1E+6, 0, 1E+3, 1E+1, true, 1E+0, 1E-4, random)

def make_pairings(teams: Seq[Team], dt: DebateType, weights: Weights): Vector[Pairing] =
  // Initiate graph
  val graph = SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  // Add vertices for teams
  for t <- 0 until teams.length do graph.addVertex(t)
  // Add weighted edges between teams within the same division
  for 
    t1 <- 0 until teams.length
    t2 <- 0 until teams.length
    if t1 != t2 
    && teams(t1).division == teams(t2).division
  do
    graph.addEdge(t1, t2)
    graph.setEdgeWeight(t1, t2, weights(teams(t1), teams(t2), dt))
  // Compute minimum-weight perfect matching, if possible
  KolmogorovWeightedPerfectMatching(graph)
    .getMatching
    .getEdges
    .asScala
    .map(
      edge => Pairing.init(
        teams(graph.getEdgeSource(edge)), 
        teams(graph.getEdgeTarget(edge)), 
        dt, 
        graph.getEdgeWeight(edge)))
    .toVector
