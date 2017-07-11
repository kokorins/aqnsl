package qn.monitor

import breeze.stats.distributions._
import qn.Resource

@Deprecated
sealed trait Monitor {
  def name:String
}

case class NamedMonitor(override val name:String) extends Monitor

sealed trait Estimation {
  def monitor:Monitor
}

case class StationaryDistributionMonitor(name:String) extends Monitor {
  def estimate(resource: Resource, stationaryDistributions: Map[Resource, DiscreteDistr[Int]]) =
    StationaryDistributionEstimation(this, stationaryDistributions(resource))
}

case class StationaryDistributionEstimation(monitor: Monitor, discreteDistr: DiscreteDistr[Int]) extends Estimation

case class ContinuousEstimation(monitor: Monitor, continuousDistribution: ContinuousDistr[Double] with HasCdf with Moments[Double, Double]) extends Estimation

case class DiscreteEstimation(monitor: Monitor, discreteDistribution: DiscreteDistr[Int]) extends Estimation

case class StatisticsEstimation(monitor: Monitor, stats: Moments[Double, Double]) extends Estimation