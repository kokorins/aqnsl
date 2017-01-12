package qn.sim

import breeze.stats.distributions.ContinuousDistr
import qn.monitor.{Estimation, Monitor}
import qn.sim.network._
import qn.util.ImmutableBiMap
import qn.{Network, NetworkTopology}

import scala.collection.mutable
import scala.util.Try

case class SimulatorArgs(networkQuery: NetworkQuery, stopAt: Double)

trait Entity {
  def receive(event: ScheduledCommand): Seq[ScheduledCommand]
}

trait ResultEntity extends Entity {
  def results: Map[Monitor, Try[Estimation]]
}

trait EstimationAppender {
  def estimate: Try[Estimation]
}

case class GeneratorEntity(receivers: List[Entity], distribution: ContinuousDistr[Double], monitors: Map[Monitor, EstimationAppender]) extends ResultEntity {
  private def generateNextOrder(time:Double): (Order, Double) = {
    (new Order(GeneratorEntity.nextId()) {}, distribution.draw() + time)
  }

  override def receive(event: ScheduledCommand): Seq[ScheduledCommand] = event match {
    case ScheduledCommand(StartSimulatorCommand, _, _, now) =>
      val (nextOrder, timeStamp) = generateNextOrder(now)
      Seq(ScheduledCommand(GenerateSimulatorCommand(nextOrder), Option(this), this :: receivers, timeStamp))
    case ScheduledCommand(GenerateSimulatorCommand(_), _, _, time) =>
      val (nextOrder, timeStamp) = generateNextOrder(time)
      Seq(ScheduledCommand(GenerateSimulatorCommand(nextOrder), Option(this), this :: receivers, timeStamp))
    case _ => Seq()
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimate)
}

object GeneratorEntity {
  var orderId: Int = 0

  def nextId(): Int = {
    orderId += 1
    orderId
  }
}

case class Order(id: Int)

sealed trait SimulatorCommand

case object StartSimulatorCommand extends SimulatorCommand

case object EndSimulatorCommand extends SimulatorCommand

case class GenerateSimulatorCommand(order: Order) extends SimulatorCommand

case class EnterSimulatorCommand(order: Order) extends SimulatorCommand

case class ProcessedSimulatorCommand(order: Order) extends SimulatorCommand

case class ScheduledCommand(event: SimulatorCommand, sender: Option[Entity], receivers: List[Entity], time: Double) {
  override def toString: String = {
    s"$event: %.2f".format(time)
  }
}

object ScheduledCommand {
  implicit def orderingByName[A <: ScheduledCommand]: Ordering[A] = Ordering.by(e => e.time)
}

case class SimulatorState(next: ScheduledCommand, events: mutable.PriorityQueue[ScheduledCommand]) {
  def isStop: Boolean = next.event match {
    case EndSimulatorCommand => true
    case _ => false
  }
}

case class Simulator(entities: List[Entity], sources: List[Entity], args: SimulatorArgs) {
  def simulate(): Try[Unit] = Try {
    var state = init(entities, sources, args)
    while(!state.isStop) {
      state = triggerNext(state)
    }
  }

  def triggerNext(state: SimulatorState): SimulatorState = {
    state.events.enqueue(state.next.receivers.flatMap(_.receive(state.next)): _*)
    if (state.events.isEmpty)
      SimulatorState(ScheduledCommand(EndSimulatorCommand, Option.empty, List(), state.next.time), state.events)
    else {
      val next = state.events.dequeue
      SimulatorState(next, state.events)
    }
  }

  def init(entities: List[Entity], sources: List[Entity], simulatorArgs: SimulatorArgs): SimulatorState = {
    val queue = mutable.PriorityQueue.newBuilder[ScheduledCommand](Ordering.by(-_.time))
    queue.enqueue(ScheduledCommand(EndSimulatorCommand, Option.empty, List(), simulatorArgs.stopAt))
    SimulatorState(ScheduledCommand(StartSimulatorCommand, Option.empty, sources, 0), queue)
  }
}

object Simulator {
  def apply(network: Network, args: SimulatorArgs): Simulator = {
    val entities = network.generators.flatMap(orderStream => {
      orderStream.trajectory match {
        case nt: NetworkTopology =>
          val nodeEntities = nt.services.map(pair => pair._1 -> NodeEntity(pair._2, NodeState(List(), pair._1.numUnits, List())))
          val networkEntity = NetworkEntity(nt, NetworkStructure(ImmutableBiMap(nodeEntities)), networkQuery = args.networkQuery)
          val generatorEntity = GeneratorEntity(List(networkEntity), orderStream.distribution, Map())
          List(networkEntity, generatorEntity) ++ nodeEntities.values
      }
    })
    Simulator(entities, entities.filter(_ match {
      case GeneratorEntity(_, _, _) => true
      case _ => false
    }), args)
  }
}
