package qn.sim.network

import breeze.stats.distributions.ContinuousDistr
import qn.sim._

import scala.collection.mutable

case class NodeState(var queue: List[Order], numSlots: Int, var processing: List[Order]) {
  def apply(diff: NodeStateEvent): NodeState = {
    val locQueue = queue.filterNot(q => diff.fromQueue.contains(q)) ::: diff.toQueue
    val locProcessing = processing.filterNot(p => diff.fromProcessing.contains(p)) ::: diff.toProcessing
    NodeState(locQueue, numSlots, locProcessing)
  }
}

case class NodeStateEvent(at: Double, toQueue: List[Order], fromQueue: Set[Order], toProcessing: List[Order], fromProcessing: Set[Order])

trait NodeQuery {
  def append(event: NodeStateEvent): Unit
}

case object EmptyNodeQuery extends NodeQuery {
  override def append(event: NodeStateEvent): Unit = {}
}

case class NodeLogger(events: mutable.ArrayBuffer[NodeStateEvent]) extends NodeQuery {
  override def append(event: NodeStateEvent): Unit = events.append(event)
}

case class NodeEntity(distribution: ContinuousDistr[Double], var state: NodeState = NodeState(List(), 1, List()), nodeQuery: NodeQuery = EmptyNodeQuery) extends Entity {
  override def receive(scheduledEvent: ScheduledCommand): Seq[ScheduledCommand] = scheduledEvent match {
    case ScheduledCommand(EnterSimulatorCommand(order), sender, _, now) =>
      if (state.processing.size < state.numSlots) {
        val event = NodeStateEvent(now, List(), Set(), List(order), Set())
        nodeQuery.append(event)
        state = state.apply(event)
        Seq(ScheduledCommand(ProcessedSimulatorCommand(order), Option(this), this :: sender.toList, now + distribution.draw()))
      } else {
        val event = NodeStateEvent(now, List(order), Set(), List(), Set())
        nodeQuery.append(event)
        state = state.apply(event)
        Seq()
      }
    case ScheduledCommand(ProcessedSimulatorCommand(order), _, receivers, now) =>
      val fromProcessing = Set(order)
      val toQueue = List()
      if (state.queue.nonEmpty) {
        val fromQueue = Set(state.queue.head)
        val toProcessing = List(state.queue.head)
        val event = NodeStateEvent(now, toQueue, fromQueue, toProcessing, fromProcessing)
        nodeQuery.append(event)
        state = state.apply(event)
        toProcessing.map(order=> ScheduledCommand(ProcessedSimulatorCommand(order), Option(this), receivers, now + distribution.draw()))
      } else {
        val event = NodeStateEvent(now, toQueue, Set(), List(), fromProcessing)
        nodeQuery.append(event)
        state = state.apply(event)
        Seq()
      }
  }
}
