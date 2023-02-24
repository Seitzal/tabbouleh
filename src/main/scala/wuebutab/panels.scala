package wuebutab

import org.jgrapht.graph._
import org.jgrapht.alg.matching.MaximumWeightBipartiteMatching

import scala.jdk.CollectionConverters._

def alloc_single(pairings: Seq[Pairing], judges: Seq[Judge], base_weight: Int = 1000): Map[Int, Int] =
  val offset = pairings.length
  val g_chair = SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  for p <- 0 until pairings.length do g_chair.addVertex(p)
  for j <- 0 until judges.length do g_chair.addVertex(offset + j)
  for p <- 0 until pairings.length; j <- 0 until judges.length do
    g_chair.addEdge(p, offset + j)
    g_chair.setEdgeWeight(p, offset + j, base_weight - judges(j).penalty(pairings(p)) * 100 + judges(j).rating)
  MaximumWeightBipartiteMatching(
    g_chair,
    (0 until pairings.length).toSet.asJava, 
    (offset until offset + judges.length).toSet.asJava
  ).getMatching()
    .getEdges()
    .asScala
    .map(edge => (g_chair.getEdgeSource(edge), g_chair.getEdgeTarget(edge) - offset))
    .toMap

def make_panels(pairings: Seq[Pairing], judges: Seq[Judge]): Seq[Panel] =
  val chairs = alloc_single(pairings, judges)
  val chairs_assigned = chairs.values.toSet
  val judges1 = (0 until judges.length).filterNot(chairs_assigned.contains).map(judges(_))
  val panelists1 = 
    if judges1.length >= pairings.length 
    then alloc_single(pairings, judges1, 90) 
    else Map()
  val panelists_assigned1 = panelists1.values.toSet
  val judges2 = (0 until judges1.length).filterNot(panelists_assigned1.contains).map(judges1(_))
  val panelists2 = 
    if judges2.length >= pairings.length
    then alloc_single(pairings, judges2, 90) 
    else Map()
  for i <- 0 until pairings.length yield Panel(
    pairings(i), 
    judges(chairs(i)), 
    panelists1.get(i).map(judges1(_)), 
    panelists2.get(i).map(judges2(_)))
