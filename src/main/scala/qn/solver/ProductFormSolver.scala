package qn.solver

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{ContinuousDistr, DiscreteDistr, Moments}
import com.typesafe.scalalogging.StrictLogging
import qn._
import qn.distribution.Distribution
import qn.monitor.{Estimation, Monitor, SojournMonitor, StationaryDistributionMonitor}

import scala.util.Try

case class ProductFormSolver(network: Network) extends StrictLogging {

  private def calcStationaryDistribution(incomingRates: DenseVector[Double],
                                         transitionMatrix: DenseMatrix[Double],
                                         serviceRates: DenseVector[Double]): Try[Seq[DiscreteDistr[Int]]] = {
    logger.info(incomingRates.toString())
    logger.info(transitionMatrix.toString())
    logger.info(serviceRates.toString())
    val resourceRates = (DenseMatrix.eye[Double](incomingRates.activeSize) - transitionMatrix).t \ incomingRates

    // Lambda = (I-Q)^-1'.L
    val loads: DenseVector[Double] = resourceRates /:/ serviceRates

    if ((loads >:= 1.0).fold(false)(_ || _)) {
      Try(throw new IllegalStateException("Network is overload: " + loads.toScalaVector().mkString(",")))
    }
    else {
      Try(loads.toArray.map(rho => Distribution.geom(1-rho)).toSeq)
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
        case NetworkTopology(_, services, _, _) => services(resource)
      }, stationaryDistribution))
      case monitor => monitor -> Try(throw new NotImplementedError(s"Monitor $monitor is not implemented yet"))
    }
    }
    val trajectoryDistributions = network.generators.flatMap(_.trajectory.monitors.map {
      case monitor => monitor -> Try(throw new NotImplementedError(s"Monitors on trajectories are not implemented"))
    })
    (netDistributions ++ resourceDistributions ++ trajectoryDistributions).toMap
  }

  private def calcStationaryDistribution(generator: OrdersStream,
                                         services: Map[Resource, ContinuousDistr[Double] with Moments[Double, Double]],
                                         transitions: Set[Transition]): Try[Map[Resource, DiscreteDistr[Int]]] = {

    val interArrivalDist = generator.distribution
    val resources = network.resources
    val lambda = 1.0 / interArrivalDist.mean
    val sourceTransitions = transitions.filter(_.from == Resource.source)
    val incomingRates = DenseVector(resources.map(resource => sourceTransitions.find(_.to == resource).fold(0.0)(_.share * lambda)).toArray)
    val serviceRates = DenseVector(resources.map(resource => resource.numUnits / services(resource).mean).toArray)
    val transitionMatrix = DenseMatrix.zeros[Double](resources.size, resources.size)
    for (i <- resources.indices; j <- resources.indices) {
      transitionMatrix.update(i, j, transitions.find(trans => trans.from == resources(i) && trans.to == resources(j)).fold(0.0)(_.share))
    }
    calcStationaryDistribution(incomingRates, transitionMatrix, serviceRates).map(_.zipWithIndex.map(pair => resources(pair._2) -> pair._1).toMap)
  }

  def solve():Try[Result] = {
    if(network.generators.size>1)
      return Try(throw new IllegalStateException("There should be only one trajectory"))
    val generator: OrdersStream = network.generators.head
    generator.trajectory match {
      case NetworkTopology(_, services, transitions, _) =>
        val stationaryDistributionTry = calcStationaryDistribution(generator, services, transitions)
        stationaryDistributionTry.map(stationaryDistribution => Result(calcMonitors(stationaryDistribution), Map()))
      case _ => Try(throw new IllegalStateException("Only transition topology is applicable"))
    }
  }

}