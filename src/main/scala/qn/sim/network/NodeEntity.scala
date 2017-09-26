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

trait Discipline {
  def receive(state: NodeState, order: Order, now: Double): NodeStateEvent
  def processed(state: NodeState, order: Order, now: Double): NodeStateEvent
}

trait Priority {
  def highest(orders: Seq[Order]): Order
}

case object Fifo extends Priority {
  override def highest(orders: Seq[Order]): Order = orders.head
}

case class NonPreemptive(priority: Priority = Fifo) extends Discipline {
  def receive(state: NodeState, order: Order, now: Double): NodeStateEvent = {
    if (state.processing.size < state.numSlots) {
      NodeStateEvent(now, List(), Set(), List(order), Set())
    } else {
      NodeStateEvent(now, List(order), Set(), List(), Set())
    }
  }
  def processed(state: NodeState, order: Order, now: Double): NodeStateEvent = {
    val fromProcessing = Set(order)
    val toQueue = List()
    if (state.queue.nonEmpty) {
      val toProcess = priority.highest(state.queue)
      val fromQueue = Set(toProcess)
      val toProcessing = List(toProcess)
      NodeStateEvent(now, toQueue, fromQueue, toProcessing, fromProcessing)
    } else {
      NodeStateEvent(now, toQueue, Set(), List(), fromProcessing)
    }
  }
}

case class NodeEntity(id: String, distribution: ContinuousDistr[Double], discipline: Discipline = NonPreemptive(),
                      var state: NodeState = NodeState(List(), 1, List()), nodeQuery: NodeQuery = EmptyNodeQuery)
  extends Entity {
  override def receive(scheduledEvent: ScheduledCommand): Seq[ScheduledCommand] = scheduledEvent match {
    case ScheduledCommand(EnterSimulatorCommand(order), sender, _, now) =>
      val event = discipline.receive(state, order, now)
      nodeQuery.append(event)
      state = state.apply(event)
      event.toProcessing.map(
        ord => ScheduledCommand(ProcessedSimulatorCommand(ord), Option(this), this :: sender.toList,
          now + distribution.draw()))
    case ScheduledCommand(ProcessedSimulatorCommand(order), _, receivers, now) =>
      val event = discipline.processed(state, order, now)
      nodeQuery.append(event)
      state = state.apply(event)
      event.toProcessing.map(
        ord => ScheduledCommand(ProcessedSimulatorCommand(ord), Option(this), receivers, now + distribution.draw()))
  }
}
