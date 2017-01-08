package qn.sim.network

import breeze.stats.distributions.ApacheContinuousDistribution
import org.apache.commons.math3.distribution.AbstractRealDistribution
import org.apache.commons.math3.random.EmpiricalDistribution
import qn.monitor.{Estimation, Monitor, SojournEstimation}
import qn.sim.{EstimationAppender, Order}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class SojournEstimationAppender(monitor: Monitor, var sample: ArrayBuffer[Double] = ArrayBuffer(), var orderStarts: mutable.Map[Order, Double] = mutable.Map()) extends EstimationAppender with NetworkQuery {
  override def estimate: Try[Estimation] = Try {
    val empiricalDistribution = new EmpiricalDistribution()
    empiricalDistribution.load(sample.toArray)
    SojournEstimation(monitor, new ApacheContinuousDistribution {
      override protected val inner: AbstractRealDistribution = empiricalDistribution
    })
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
