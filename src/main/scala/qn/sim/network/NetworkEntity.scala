package qn.sim.network

import breeze.stats.distributions.{ApacheContinuousDistribution, ContinuousDistr}
import org.apache.commons.math3.distribution.AbstractRealDistribution
import org.apache.commons.math3.random.EmpiricalDistribution
import qn.{NetworkTopology, Resource}
import qn.distribution.Distribution
import qn.monitor.{Estimation, Monitor, SojournEstimation}
import qn.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class NetworkStateDiff(at: Double, networkIn: Set[Order], networkOut: Set[Order]) extends StateUpdate
case class NetworkState(nodeEntities: Map[Resource, NodeEntity]) {}

case class NetworkEntity(networkTopology: NetworkTopology, monitors: Map[Monitor, EstimationAppender], state: NetworkState) extends ResultEntity {
  override def receive(event: ScheduledCommand): Seq[ScheduledCommand] = event match {
    case ScheduledCommand(GenerateSimulatorCommand(order), _, _, time) =>
      val sources = networkTopology.transitions.filter(_.from == Resource.source).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = state.nodeEntities(sources(dist.draw()).to)
      Seq(ScheduledCommand(EnterSimulatorCommand(order), Option(this), List(to), time))
    case ScheduledCommand(ProcessedSimulatorCommand(order), node, _, time) =>
      val sources = networkTopology.transitions.filter(_.from == node.get).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = state.nodeEntities(sources(dist.draw()).to)
      Seq(ScheduledCommand(EnterSimulatorCommand(order), Option(this), List(to), time))
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)
}

case class SojournEstimationAppender(monitor: Monitor, var sample: ArrayBuffer[Double], var orderStarts: mutable.Map[Order, Double]) extends EstimationAppender {
  override def estimator: Try[Estimation] = Try {
                                                  val empiricalDistribution = new EmpiricalDistribution()
                                                  empiricalDistribution.load(sample.toArray)
                                                  SojournEstimation(monitor, new ApacheContinuousDistribution {
                                                    override protected val inner: AbstractRealDistribution = empiricalDistribution
                                                  })
                                                }

  override def warnings: String = ""

  override def append(update: StateUpdate): EstimationAppender = update match {
    case NetworkStateDiff(at, itemIn, itemOut) => {
      for (o <- itemOut) {
        sample += at - orderStarts(o)
      }
      for (o <- itemIn) {
        orderStarts += o -> at
      }
      this
    }
  }
}