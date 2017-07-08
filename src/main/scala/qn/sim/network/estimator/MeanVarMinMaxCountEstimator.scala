package qn.sim.network.estimator

import breeze.stats.distributions.Moments
import qn.monitor.{Estimation, Monitor, NamedMonitor, StatisticsEstimation}
import qn.sim.network.{NetworkQuery, NetworkStateEvent, NodeQuery, NodeStateEvent}
import qn.sim.{Estimator, Order}

import scala.collection.mutable
import scala.util.Try

case class MeanVarMinMaxCountEstimator(mean: Double, v: Double, min: Double, max: Double, count: Int) {
  def add(x: Double): MeanVarMinMaxCountEstimator = {
    if (count == 0) {
      return MeanVarMinMaxCountEstimator(x, 0, x, x, 1)
    }
    val newMean = mean + (x - mean) / (count + 1)
    val newVar = (count * (v + math.pow(mean - newMean, 2)) + math.pow(x - newMean, 2)) / (count + 1)
    MeanVarMinMaxCountEstimator(newMean, newVar, math.min(min, x), math.max(max, x), count + 1)
  }
}

case class SojournMomentsEstimator(monitor: Monitor, var adder: MeanVarMinMaxCountEstimator,
                                   ordersIn: mutable.Map[Order, Double])
  extends Estimator with NetworkQuery with NodeQuery {
  override def estimate: Try[Estimation] = Try {
    StatisticsEstimation(monitor, new Moments[Double, Double] {
      override def mean: Double = adder.mean
      override def variance: Double = adder.v
      override def entropy: Double = ???
      override def mode: Double = adder.mean
    })
  }
  override def append(event: NetworkStateEvent): Unit = {
    for (o <- event.networkOut) {
      adder = adder.add(event.at - ordersIn(o))
    }
    for (o <- event.networkIn) {
      ordersIn += o -> event.at
    }
  }
  override def append(event: NodeStateEvent): Unit = {
    for (o <- event.fromProcessing) {
      adder = adder.add(event.at - ordersIn(o))
    }
    for (o <- event.toQueue) {
      ordersIn += o -> event.at
    }
    for (o <- event.toProcessing if !event.fromQueue.contains(o)) {
      ordersIn += o -> event.at
    }
  }
}

object SojournMomentsEstimator {
  def apply(name: String): SojournMomentsEstimator = SojournMomentsEstimator(NamedMonitor(name),
    MeanVarMinMaxCountEstimator(0, 0, 0, 0, 0), mutable.Map())
}
