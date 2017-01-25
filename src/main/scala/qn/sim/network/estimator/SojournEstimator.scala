package qn.sim.network.estimator

import breeze.stats.distributions.ApacheContinuousDistribution
import org.apache.commons.math3.distribution.AbstractRealDistribution
import org.apache.commons.math3.random.EmpiricalDistribution
import qn.monitor.{ContinuousEstimation, Estimation, Monitor, NamedMonitor}
import qn.sim.network.{NetworkQuery, NetworkStateEvent}
import qn.sim.{Estimator, Order}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class SojournEstimator(monitor: Monitor, sample: ArrayBuffer[Double], orderStarts: mutable.Map[Order, Double])
  extends Estimator with NetworkQuery {

  override def estimate: Try[Estimation] = Try {
    val empiricalDistribution = new EmpiricalDistribution()
    empiricalDistribution.load(sample.toArray)
    val res = ContinuousEstimation(monitor, new ApacheContinuousDistribution {
      override protected val inner: AbstractRealDistribution = empiricalDistribution

      override def toString: String = s"${inner.getClass.getSimpleName}(mean: ${this.mean}, variance: ${this.variance})"
    })
    res
  }

  override def append(event: NetworkStateEvent): Unit = {
    for (o <- event.networkOut) {
      sample += event.at - orderStarts(o)
    }
    for (o <- event.networkIn) {
      orderStarts += o -> event.at
    }
  }
}

object SojournEstimator {
  def apply(name: String): SojournEstimator = SojournEstimator(NamedMonitor(name), ArrayBuffer(), mutable.Map())
}
