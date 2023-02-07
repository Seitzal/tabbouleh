package wuebutab

import com.github.tototoshi.csv._

import org.jgrapht.graph._
import org.jgrapht.alg.matching.blossom.v5.KolmogorovWeightedPerfectMatching

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

enum Side(val symbol: String):
  case Proposition extends Side("P")
  case Opposition extends Side("O")

enum DebateType(val symbol: String):
  case Impromptu extends DebateType("Impr")
  case Prepared extends DebateType("Prep")

case class Team(
  val id: Int,
  val kv: Map[String, String]):

  def name: String = kv("Team")
  def wins: Int = kv("Wins").toInt
  def ballots: Int = kv("Ballots").toInt
  def points: Double = kv("Points").toDouble
  def previous_opponents: Set[String] = 
    kv.keys.filter(_.startsWith("Opponent")).map(kv).toSet.filter(!_.isBlank)
  override def toString(): String = s"$id\t$name"
  def side_pref(dt: DebateType) = 
    Integer.parseInt(kv("Balance")) match
      case 0 => Integer.parseInt(kv(dt.symbol)) / 2
      case p => p
  def rank_score: Double =
    wins + ballots * 1E-2 + points * 1E-6

def read_team_table(loc: String) =
  val reader = CSVReader.open(java.io.File(loc))
  val raw = reader.iteratorWithHeaders.toVector
  reader.close()
  (0 until raw.length).toVector.map(i => Team(i, raw(i)))

def side_imbalance(t1: Team, t2: Team, dt: DebateType) =
  if (t1.side_pref(dt) > 0 && t2.side_pref(dt) > 0) || (t1.side_pref(dt) < 0 && t2.side_pref(dt) < 0) 
  then math.abs(t1.side_pref(dt) + t2.side_pref(dt))
  else 0

case class Weights(
  rematch: Double = 1E+9,
  side_pref: Double = 1E+6,
  side_nofix: Double = 0,
  wins: Double = 1E+1,
  ballots: Double = 1E+0,
  points: Double = 1E-4
)

case class Pairing(
  t1: Team,
  t2: Team,
  dt: DebateType,
  weight: Double):

  val ran = Random.nextBoolean()

  def prop: Team = 
    if t1.side_pref(dt) == t2.side_pref(dt) && ran then t2
    else if t1.side_pref(dt) > t2.side_pref(dt) then t2
    else t1
  
  def opp: Team = if prop == t1 then t2 else t1

  def rematch: Boolean = t1.previous_opponents.contains(t2.name)

  def mean_rank_score: Double = (t1.rank_score + t2.rank_score) / 2

def make_pairings(teams: Seq[Team], dt: DebateType, weights: Weights = Weights()) =
  // Initialize a weighted undirected graph to represent the possible pairings
  val graph = SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  // Add a vertex to the graph for each team
  for t <- 0 until teams.length do graph.addVertex(t)
  // Weighted edges represent all possible pairings
  for 
    t1 <- 0 until teams.length
    t2 <- 0 until teams.length 
    if t1 != t2 
  do
    // Add an edge to the graph
    graph.addEdge(t1, t2)
    // Set the weight of the edge
    graph.setEdgeWeight(t1, t2, 0d +
      (if teams(t1).previous_opponents.contains(teams(t2).name) then weights.rematch else 0) +
      side_imbalance(teams(t1), teams(t2), dt) * weights.side_pref +
      (if teams(t1).side_pref(dt) == 0 && teams(t2).side_pref(dt) == 0 then weights.side_nofix else 0) +
      math.pow(10, math.abs(teams(t1).wins - teams(t2).wins)) * weights.wins +
      math.abs(teams(t1).ballots - teams(t2).ballots) * weights.ballots +
      math.abs(teams(t1).points - teams(t2).points) * weights.points)
  // Use Kolmogorov's algorithm to find a minimum-weight perfect matching for the graph
  KolmogorovWeightedPerfectMatching(graph)
    .getMatching
    .getEdges
    .asScala
    .map(edge => Pairing(teams(graph.getEdgeSource(edge)), teams(graph.getEdgeTarget(edge)), dt, graph.getEdgeWeight(edge)))
    .toVector

@main def test: Unit =
  val teams = wuebutab.read_team_table("teams.csv")
  val pairings = make_pairings(teams, DebateType.Prepared).sortBy(_.mean_rank_score).reverse
  println(pairings.length)
  print_table(pairings)
