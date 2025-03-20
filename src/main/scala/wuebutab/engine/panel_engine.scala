package wuebutab

import org.jgrapht.graph._
import org.jgrapht.alg.matching.MaximumWeightBipartiteMatching
import scala.util.Random
import scala.jdk.CollectionConverters._

case class PanelWeights(
  base_chair: Double = 1000,
  base_panelist: Double = 90,
  penalty: Double = 100,
  rating_chair: Double = 1,
  rotation_chair: Double = .75,
  rating_panelist: Double = 0,
  new_panel: Double = 1,
  bubble: Double = 0.1,
  top: Double = 0.05,
  random: Double = 0.005):
  def apply(j: Judge, p: Pairing, mmrs: Double, chair: Boolean, panel: Seq[String]): Double =
    (if chair then base_chair else base_panelist)
    - j.penalty(p) * penalty
    + j.rating * (if chair then rating_chair else rating_panelist)
    - (if chair then j.times_chair * rotation_chair else 0)
    - math.abs(p.mean_rank_score - mmrs) * bubble
    + (if panel.filter(j.colleagues_prev.contains).isEmpty then new_panel else 0)
    + p.mean_rank_score * top
    + Random.nextDouble() * random

def alloc_single(pairings: Seq[Pairing], panels: Seq[Seq[String]], judges: Seq[Judge], weights: PanelWeights, chair: Boolean): Map[Int, Int] =
  val offset = pairings.length
  val mmrs = pairings.map(_.mean_rank_score).sum / offset
  val graph = SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  for p <- 0 until pairings.length do graph.addVertex(p)
  for j <- 0 until judges.length do graph.addVertex(offset + j)
  for 
    p <- 0 until pairings.length
    j <- 0 until judges.length 
    if judges(j).division == pairings(p).division 
  do
    graph.addEdge(p, offset + j)
    graph.setEdgeWeight(p, offset + j, weights(judges(j), pairings(p), mmrs, chair, panels(p)))
  MaximumWeightBipartiteMatching(
    graph,
    (0 until pairings.length).toSet.asJava, 
    (offset until offset + judges.length).toSet.asJava
  ).getMatching()
    .getEdges()
    .asScala
    .map(edge => (graph.getEdgeSource(edge), graph.getEdgeTarget(edge) - offset))
    .toMap

def make_panels(pairings: Seq[Pairing], judges: Seq[Judge], weights: PanelWeights = PanelWeights()): Vector[Panel] =
  val panels0 = pairings.map(_ => Vector())
  val chairs = alloc_single(pairings, panels0, judges, weights, true)
  val chairs_assigned = chairs.values.toSet
  val panels1 = for i <- 0 until panels0.length yield 
    if chairs.isDefinedAt(i) 
      then Vector(judges(chairs(i)).name) else Vector()
  val judges1 = (0 until judges.length).filterNot(chairs_assigned.contains).map(judges(_))
  val panelists1 = alloc_single(pairings, panels1, judges1, weights, false) 
  val panelists_assigned1 = panelists1.values.toSet
  val panels2 = for i <- 0 until panels1.length yield 
    if panelists1.isDefinedAt(i) 
      then panels0(i) :+ judges(panelists1(i)).name else Vector()
  val judges2 = (0 until judges1.length).filterNot(panelists_assigned1.contains).map(judges1(_))
  val panelists2 = alloc_single(pairings, panels2, judges2, weights, false) 
  for i <- (0 until pairings.length).toVector yield Panel(
    pairings(i), 
    judges(chairs(i)), 
    panelists1.get(i).map(judges1(_)), 
    panelists2.get(i).map(judges2(_)))
