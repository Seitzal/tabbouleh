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
  rating_panelist: Double = 0,
  bubble: Double = 0.1,
  top: Double = 0.05,
  random: Double = 0.005):
  def apply(j: Judge, p: Pairing, mmrs: Double, chair: Boolean): Double =
    (if chair then base_chair else base_panelist)
    - j.penalty(p) * penalty
    + j.rating * (if chair then rating_chair else rating_panelist)
    - math.abs(p.mean_rank_score - mmrs) * bubble
    + p.mean_rank_score * top
    + Random.nextDouble() * random

def alloc_single(pairings: Seq[Pairing], judges: Seq[Judge], weights: PanelWeights, chair: Boolean): Map[Int, Int] =
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
    graph.setEdgeWeight(p, offset + j, weights(judges(j), pairings(p), mmrs, chair))
  MaximumWeightBipartiteMatching(
    graph,
    (0 until pairings.length).toSet.asJava, 
    (offset until offset + judges.length).toSet.asJava
  ).getMatching()
    .getEdges()
    .asScala
    .map(edge => (graph.getEdgeSource(edge), graph.getEdgeTarget(edge) - offset))
    .toMap

def make_panels(pairings: Seq[Pairing], judges: Seq[Judge], weights: PanelWeights = PanelWeights()): Seq[Panel] =
  val chairs = alloc_single(pairings, judges, weights, true)
  val chairs_assigned = chairs.values.toSet
  val judges1 = (0 until judges.length).filterNot(chairs_assigned.contains).map(judges(_))
  val panelists1 = alloc_single(pairings, judges1, weights, false) 
  val panelists_assigned1 = panelists1.values.toSet
  val judges2 = (0 until judges1.length).filterNot(panelists_assigned1.contains).map(judges1(_))
  val panelists2 = alloc_single(pairings, judges2, weights, false) 
  for i <- 0 until pairings.length yield Panel(
    pairings(i), 
    judges(chairs(i)), 
    panelists1.get(i).map(judges1(_)), 
    panelists2.get(i).map(judges2(_)))
