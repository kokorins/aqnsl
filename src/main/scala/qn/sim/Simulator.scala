package qn.sim

import breeze.stats.distributions.ContinuousDistr
import qn.Network
import qn.monitor.{Estimation, Monitor}
import qn.solver.Result

import scala.collection.mutable
import scala.util.Try

case class SimulatorArgs(stopAt: Double)

trait Entity {
  def receive(event: ScheduledEvent): Seq[ScheduledEvent]

  def results: Map[Monitor, Try[Estimation]]

  def warnings: Map[Monitor, String]
}

trait EstimationAppender {
  def estimator: Try[Estimation]

  def warnings: String
}

case class GeneratorEntity(distribution: ContinuousDistr[Double], monitors: Map[Monitor, EstimationAppender]) extends Entity {
  private def generateNextOrder(time:Double): (Order, Double) = {
    (new Order() {}, distribution.draw() + time)
  }
  override def receive(event: ScheduledEvent): Seq[ScheduledEvent] = event match {


    case ScheduledEvent(StartEvent, _, time) =>
      val (nextOrder, timeStamp) = generateNextOrder(time)
      Seq(ScheduledEvent(GenerateEvent(nextOrder), Option(this), time + time))
    case ScheduledEvent(GenerateEvent(order), sender, time) =>
    case _ => Seq()
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
}

trait Order

trait Event

case object StartEvent extends Event

case object EndEvent extends Event

case class GenerateEvent(order: Order) extends Event

case class ScheduledEvent(event: Event, sender: Option[Entity], time: Double)

case class SimulatorState(next: ScheduledEvent, events: mutable.PriorityQueue[ScheduledEvent]) {
  def isStop: Boolean = next.event match {
    case EndEvent => true
    case _ => false
  }
}

case class Simulator(entities: Seq[Entity], args: SimulatorArgs) {
  def simulate(): Try[Result] = Try {
    var state = init(entities, args)
    while(!state.isStop) {
      state = triggerNext(state)
    }
    entities.foldLeft(Result(Map(), Map()))((lhs, rhs) => {
      lhs.copy(lhs.results ++ rhs.results, lhs.warnings ++ rhs.warnings)
    })
  }

  def triggerNext(state: SimulatorState) = {
    state.events.enqueue(entities.flatMap(_.receive(state.next)): _*)
    val next = state.events.dequeue
    SimulatorState(next, state.events)
  }


  def init(entities: Seq[Entity], simulatorArgs: SimulatorArgs): SimulatorState = {
    val queue = mutable.PriorityQueue.newBuilder[ScheduledEvent](Ordering.by(_.time))
    queue.enqueue(ScheduledEvent(EndEvent, Option.empty, simulatorArgs.stopAt))
    SimulatorState(ScheduledEvent(StartEvent, Option.empty, 0), queue)
  }
}

object Simulator {
  def apply(network: Network, args: SimulatorArgs) = {

  }
}
