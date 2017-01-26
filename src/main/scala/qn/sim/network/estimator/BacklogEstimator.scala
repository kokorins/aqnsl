package qn.sim.network.estimator

import breeze.stats.distributions.ApacheDiscreteDistribution
import org.apache.commons.math3.distribution.{AbstractIntegerDistribution, EnumeratedIntegerDistribution}
import qn.Resource
import qn.monitor.{DiscreteEstimation, Estimation, Monitor, NamedMonitor}
import qn.sim.Estimator
import qn.sim.network.{NodeQuery, NodeStateEvent}

import scala.collection.mutable
import scala.util.Try


case class BacklogEstimator(monitor: Monitor, var curBacklog: Int, var lastChange: Double, length: mutable.Map[Int, Double]) extends Estimator with NodeQuery {
  override def estimate: Try[Estimation] = Try {
    val (keys, values) = length.unzip
    val distribution = new ApacheDiscreteDistribution {
      override protected val inner: AbstractIntegerDistribution = new EnumeratedIntegerDistribution(keys.toArray, values.map(_ / lastChange).toArray)

      override def toString: String = s"${inner.getClass.getSimpleName}(${length.mapValues(_ / lastChange)})"
    }
    DiscreteEstimation(monitor, distribution)
  }

  override def append(event: NodeStateEvent): Unit = {
    val backlogDiff = event.toQueue.size + event.toProcessing.size - event.fromQueue.size - event.fromProcessing.size
    var cur = length.getOrElse(curBacklog, 0.0)
    cur += event.at - lastChange
    length.put(curBacklog, cur)
    curBacklog += backlogDiff
    lastChange = event.at
  }
}

object BacklogEstimator {
  def apply(resource: Resource): BacklogEstimator = BacklogEstimator(NamedMonitor(resource.name), 0, 0, mutable.Map())
}
