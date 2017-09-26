package qn.sim.network.estimator

import qn.distribution.Singular
import qn.sim.network.{NetworkQuery, NetworkStateEvent}

import scala.util.Try

case class ProcessedEstimator(name: String, var counter: Double) extends NetworkQuery {

  def estimate: Try[Singular] = Try {
    Singular(counter)
  }

  override def append(event: NetworkStateEvent): Unit = {
    counter += event.networkOut.size
  }
}

object ProcessedEstimator {
  def apply(name: String): ProcessedEstimator = new ProcessedEstimator(name, 0.0)
}