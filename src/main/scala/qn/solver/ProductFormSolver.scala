package qn.solver

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{ContinuousDistr, DiscreteDistr, Moments}
import com.typesafe.scalalogging.StrictLogging
import qn._
import qn.distribution.Distribution
import qn.monitor.{Estimation, Monitor, SojournMonitor, StationaryDistributionMonitor}
import qn.util.ImmutableBiMap

import scala.util.Try
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

case class ProductFormSolver(network: Network) extends StrictLogging {

  private def calcStationaryDistribution(incomingRates: DenseVector[Double],
                                         transitionMatrix: DenseMatrix[Double],
                                         serviceRates: DenseVector[Double]): Try[Seq[DiscreteDistr[Int]]] = {
    logger.info(s"Node incoming rates: $incomingRates")
    logger.info(s"Transition matrix: $transitionMatrix")
    logger.info(s"Service rates: $serviceRates")
    val resourceRates = (DenseMatrix.eye[Double](incomingRates.activeSize) - transitionMatrix).t \ incomingRates

    // Lambda = (I-Q)^-1'.L
    val loads: DenseVector[Double] = resourceRates /:/ serviceRates

    if ((loads >:= 1.0).fold(false)(_ || _)) {
      Try(throw new IllegalStateException("Network is overload: " + loads.toScalaVector().mkString(",")))
    }
    else {
      Try(loads.toArray.toSeq.map(rho => Distribution.geom(1 - rho)))
    }
  }


  private def calcMonitors(stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Map[Monitor, Try[Estimation]] = {
    val netDistributions = network.monitors.map {
      case monitor: SojournMonitor => monitor -> Try(monitor.estimate(network, stationaryDistribution))
      case monitor => monitor -> Try(throw new NotImplementedError(s"Monitor $monitor is not implemented yet"))
    }
    val resourceDistributions = network.resources.flatMap { resource => resource.monitors.map {
      case monitor: StationaryDistributionMonitor => monitor -> Try(monitor.estimate(resource, stationaryDistribution))
      case monitor: SojournMonitor => monitor -> Try(monitor.estimate(resource, network.generators.head.trajectory match {
        case NetworkGraph(_, services, _, _) => services(resource)
      }, stationaryDistribution))
      case monitor => monitor -> Try(throw new NotImplementedError(s"Monitor $monitor is not implemented yet"))
    }
    }
    val trajectoryDistributions = network.generators.flatMap(_.trajectory.monitors.map(monitor => monitor -> Try(throw new NotImplementedError(s"Monitors on trajectories are not implemented"))))
    (netDistributions ++ resourceDistributions ++ trajectoryDistributions).toMap
  }

  private def calcStationaryDistribution(generator: OrdersStream,
                                         services: Map[Resource, ContinuousDistr[Double] with Moments[Double, Double]],
                                         graph: Graph[Resource, WDiEdge]): Try[Map[Resource, DiscreteDistr[Int]]] = {

    val interArrivalDist = generator.distribution
    val seqResources = network.resources.zipWithIndex

    val lambda = 1.0 / interArrivalDist.mean
    val sourceTransitions = graph.get(Resource.source).outgoing
    val incomingRates = DenseVector(seqResources.map(resource => sourceTransitions.find(_.to == resource._1).fold(0.0)(_.weight / Long.MaxValue * lambda)).toArray)
    val serviceRates = DenseVector(seqResources.map(_._1).map(resource => resource.numUnits / services(resource).mean).toArray)

    val transitionMatrix = DenseMatrix.zeros[Double](seqResources.size, seqResources.size)

    val resourcesToIdxs = ImmutableBiMap(seqResources.toMap)
    for {
      e <- graph.edges
    } transitionMatrix.update(resourcesToIdxs(e.from), resourcesToIdxs(e.to), e.weight / Long.MaxValue)

    val idxToResources = resourcesToIdxs.inverse
    calcStationaryDistribution(incomingRates, transitionMatrix, serviceRates).map(_.zipWithIndex.map(pair => (idxToResources(pair._2), pair._1)).toMap)
  }

  def solve():Try[Result] = {
    if(network.generators.size>1)
      return Try(throw new IllegalStateException("There should be only one trajectory"))
    val generator: OrdersStream = network.generators.head
    generator.trajectory match {
      case NetworkGraph(_, services, graph, _) =>
        val stationaryDistributionTry = calcStationaryDistribution(generator, services, graph)
        stationaryDistributionTry.map(stationaryDistribution => Result(calcMonitors(stationaryDistribution), Map()))
      case _ => Try(throw new IllegalStateException("Only transition topology is applicable"))
    }
  }

}