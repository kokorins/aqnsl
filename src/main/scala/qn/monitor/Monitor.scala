package qn.monitor

import breeze.stats.distributions.{ContinuousDistr, DiscreteDistr, Exponential, Geometric}
import qn.Resource
import qn.distribution.Distribution

import scala.util.Try

sealed trait Monitor {
  def name:String
}

sealed trait Estimation {
  def monitor:Monitor
}

case class StationaryDistributionMonitor(name:String) extends Monitor {
  def estimate(resource: Resource, stationaryDistributions: Map[Resource, DiscreteDistr[Int]]) =
    StationaryDistributionEstimation(this, stationaryDistributions(resource))
}

case class StationaryDistributionEstimation(monitor: Monitor, discreteDistr: DiscreteDistr[Int]) extends Estimation

case class SojournMonitor(name:String) extends Monitor {
  def estimate(resource: Resource, service: ContinuousDistr[Double], stationaryDistribution: Map[Resource, DiscreteDistr[Int]]) =
    service match {
      case dist:Exponential => {
        stationaryDistribution(resource) match {
          case geom:Geometric => Try(SojournEstimation(this, Distribution.sumRandom(dist, geom)))
        }
      }
      case _ => Try(throw new IllegalArgumentException(s"Sojourn distribution is not implemented for such service distribution $service"))
    }
}

case class SojournEstimation(monitor: Monitor, continuousDistr: ContinuousDistr[Double]) extends Estimation