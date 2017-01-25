package qn.sim.network.estimator

import qn.distribution.Singular
import qn.monitor.{ContinuousEstimation, Estimation, Monitor, NamedMonitor}
import qn.sim.Estimator
import qn.sim.network.{NetworkQuery, NetworkStateEvent}

import scala.util.Try

case class ProcessedEstimator(monitor: Monitor, var counter: Double) extends Estimator with NetworkQuery {

  override def estimate: Try[Estimation] = Try {
    ContinuousEstimation(monitor, Singular(counter))
  }

  override def append(event: NetworkStateEvent): Unit = {
    counter += event.networkOut.size
  }
}

object ProcessedEstimator {
  def apply(name: String): ProcessedEstimator = new ProcessedEstimator(NamedMonitor(name), 0.0)
}