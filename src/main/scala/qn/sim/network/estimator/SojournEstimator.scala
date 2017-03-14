package qn.sim.network.estimator

import breeze.stats.distributions.{ApacheContinuousDistribution, Moments}
import com.typesafe.scalalogging.Logger
import org.apache.commons.math3.distribution.AbstractRealDistribution
import org.apache.commons.math3.random.EmpiricalDistribution
import qn.monitor.{ContinuousEstimation, Estimation, Monitor, NamedMonitor}
import qn.sim.network.{NetworkQuery, NetworkStateEvent, NodeQuery, NodeStateEvent}
import qn.sim.{Estimator, Order}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class SojournEstimator(monitor: Monitor, sample: ArrayBuffer[Double], orderStarts: mutable.Map[Order, Double])
  extends Estimator with NetworkQuery with NodeQuery {
  private val logger = Logger[SojournEstimator]

  override def estimate: Try[Estimation] = Try {
    val empiricalDistribution = new EmpiricalDistribution(sample.size / 5)
    empiricalDistribution.load(sample.toArray)
    logger.info(sample.toString())
    val res = ContinuousEstimation(monitor, new ApacheContinuousDistribution with Moments[Double, Double] {
      override protected val inner: AbstractRealDistribution = empiricalDistribution

      override def toString: String = s"${inner.getClass.getSimpleName}(mean: ${this.mean}, variance: ${this.variance})"

      override def entropy: Double = ???

      override def mode: Double = ???
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

  override def append(event: NodeStateEvent): Unit = {
    for (o <- event.fromProcessing) {
      sample += event.at - orderStarts(o)
    }
    for (o <- event.toQueue) {
      orderStarts += o -> event.at
    }
    for (o <- event.toProcessing if !event.fromQueue.contains(o)) {
      orderStarts += o -> event.at
    }
  }
}

object SojournEstimator {
  def apply(name: String): SojournEstimator = SojournEstimator(NamedMonitor(name), ArrayBuffer(), mutable.Map())
}
