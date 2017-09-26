package qn

import breeze.stats.distributions.{ContinuousDistr, Moments}

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

sealed trait Trajectory {
  def transitions: Set[Transition]
}

case class Transition(from:Resource, to:Resource, share:Double)

case class NetworkGraph(name: String = "",
                        services: Map[Resource, ContinuousDistr[Double] with Moments[Double, Double]] = Map(),
                        graph: Graph[Resource, WDiEdge] = Graph()) extends Trajectory {

  def addTransition(from: Resource, to: Resource): NetworkGraph = copy(
    graph = graph.+(WDiEdge(from, to)(Long.MaxValue)))
  def addShares(from: Resource, pairs: (Resource, Double)*): NetworkGraph = copy(
    graph = graph ++ pairs.map(p => WDiEdge(from, p._1)(math.round(p._2 * Long.MaxValue))))
  def addService(resource: Resource,
                 distribution: ContinuousDistr[Double] with Moments[Double, Double]): NetworkGraph = copy(
    services = services + (resource -> distribution))

  override def transitions: Set[Transition] = graph.edges
    .map(edge => Transition(edge.source.value, edge.target.value, edge.weight.toDouble / Long.MaxValue)).toSet
}