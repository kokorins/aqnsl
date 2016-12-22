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

case class NetworkEntity(networkTopology: NetworkTopology, monitors: Map[Monitor, EstimationAppender], state: NetworkState) extends Entity {
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

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
}

case class NodeState(var queue: List[Order], numSlots: Int, var processing: List[Order]) {
  def apply(diff: NodeStateDiff): NodeState = {
    val locQueue = queue.filter(q => diff.fromQueue.contains(q)) ::: diff.toQueue
    val locProcessing = processing.filter(p => diff.fromProcessing.contains(p)) ::: diff.toProcessing
    NodeState(locQueue, numSlots, locProcessing)
  }
}

case class NodeStateDiff(at: Double, toQueue: List[Order], fromQueue: Set[Order], toProcessing: List[Order], fromProcessing: Set[Order])

case class NodeEntity(distribution: ContinuousDistr[Double], monitors: Map[Monitor, EstimationAppender], var state: NodeState) extends Entity {
  override def receive(event: ScheduledCommand): Seq[ScheduledCommand] = event match {
    case ScheduledCommand(EnterSimulatorCommand(order), sender, _, now) =>
      if (state.processing.size < state.numSlots) {
        state = state.apply(NodeStateDiff(now, List(), Set(), List(order), Set()))
        Seq(ScheduledCommand(ProcessedSimulatorCommand(order), Option(this), this :: sender.toList, now + distribution.draw()))
      } else {
        state = state.apply(NodeStateDiff(now, List(order), Set(), List(), Set()))
        Seq()
      }
    case ScheduledCommand(ProcessedSimulatorCommand(order), _, receivers, now) =>
      val fromProcessing = Set(order)
      val toQueue = List()
      if (state.queue.nonEmpty) {
        val fromQueue = Set(state.queue.head)
        val toProcessing = List(state.queue.head)
        state = state.apply(NodeStateDiff(now, toQueue, fromQueue, toProcessing, fromProcessing))
        Seq(ScheduledCommand(ProcessedSimulatorCommand(order), Option(this), receivers, now + distribution.draw()))
      } else {
        state = state.apply(NodeStateDiff(now, toQueue, Set(), List(), fromProcessing))
        Seq()
      }
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
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