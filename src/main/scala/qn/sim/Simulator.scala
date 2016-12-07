package qn.sim

import breeze.stats.distributions.{ApacheContinuousDistribution, ContinuousDistr}
import org.apache.commons.math3.distribution.AbstractRealDistribution
import org.apache.commons.math3.random.EmpiricalDistribution
import qn.distribution.Distribution
import qn.monitor.{Estimation, Monitor, SojournEstimation, SojournMonitor}
import qn.solver.Result
import qn.{Network, NetworkTopology, Resource}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class SimulatorArgs(stopAt: Double)

trait Entity {
  def receive(event: ScheduledEvent): Seq[ScheduledEvent]

  def results: Map[Monitor, Try[Estimation]]

  def warnings: Map[Monitor, String]
}

sealed trait StateUpdate

trait EstimationAppender {
  def append(update: StateUpdate): EstimationAppender
  def estimator: Try[Estimation]
  def warnings: String
}

case class GeneratorEntity(receivers: List[Entity], distribution: ContinuousDistr[Double], monitors: Map[Monitor, EstimationAppender]) extends Entity {
  private def generateNextOrder(time:Double): (Order, Double) = {
    (new Order() {}, distribution.draw() + time)
  }
  override def receive(event: ScheduledEvent): Seq[ScheduledEvent] = event match {
    case ScheduledEvent(StartEvent, _, _, now) =>
      val (nextOrder, timeStamp) = generateNextOrder(now)
      Seq(ScheduledEvent(GenerateEvent(nextOrder), Option(this), this :: receivers, timeStamp))
    case ScheduledEvent(GenerateEvent(_), _, _, time) =>
      val (nextOrder, timeStamp) = generateNextOrder(time)
      Seq(ScheduledEvent(GenerateEvent(nextOrder), Option(this), this :: receivers, timeStamp))
    case _ => Seq()
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
}

case class NetworkStateDiff(at: Double, networkIn: Set[Order], networkOut: Set[Order]) extends StateUpdate
case class NetworkState(nodeEntities: Map[Resource, NodeEntity]) {}

case class NetworkEntity(networkTopology: NetworkTopology, monitors: Map[Monitor, EstimationAppender], state: NetworkState) extends Entity {
  override def receive(event: ScheduledEvent): Seq[ScheduledEvent] = event match {
    case ScheduledEvent(GenerateEvent(order), _, _, time) =>
      val sources = networkTopology.transitions.filter(_.from == Resource.source).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = state.nodeEntities(sources(dist.draw()).to)
      Seq(ScheduledEvent(EnterEvent(order), Option(this), List(to), time))
    case ScheduledEvent(ProcessedEvent(order), node, _, time) =>
      val sources = networkTopology.transitions.filter(_.from == node.get).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = state.nodeEntities(sources(dist.draw()).to)
      Seq(ScheduledEvent(EnterEvent(order), Option(this), List(to), time))
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
}

case class NodeState(var queue: List[Order], numSlots: Int, var processing: List[Order]) {
  def apply(diff: NodeStateDiff) = {
    val locQueue = queue.filter(q => diff.fromQueue.contains(q)) ::: diff.toQueue
    val locProcessing = processing.filter(p => diff.fromProcessing.contains(p)) ::: diff.toProcessing
    NodeState(locQueue, numSlots, locProcessing)
  }
}

case class NodeStateDiff(at: Double, toQueue: List[Order], fromQueue: Set[Order], toProcessing: List[Order], fromProcessing: Set[Order])

case class NodeEntity(distribution: ContinuousDistr[Double], monitors: Map[Monitor, EstimationAppender], var state: NodeState) extends Entity {
  override def receive(event: ScheduledEvent): Seq[ScheduledEvent] = event match {
    case ScheduledEvent(EnterEvent(order), sender, _, now) =>
      if (state.processing.size < state.numSlots) {
        state = state.apply(NodeStateDiff(now, List(), Set(), List(order), Set()))
        Seq(ScheduledEvent(ProcessedEvent(order), Option(this), this :: sender.toList, now + distribution.draw()))
      } else {
        state = state.apply(NodeStateDiff(now, List(order), Set(), List(), Set()))
        Seq()
      }
    case ScheduledEvent(ProcessedEvent(order), _, receivers, now) =>
      val fromProcessing = Set(order)
      val toQueue = List()
      if (state.queue.nonEmpty) {
        val fromQueue = Set(state.queue.head)
        val toProcessing = List(state.queue.head)
        state = state.apply(NodeStateDiff(now, toQueue, fromQueue, toProcessing, fromProcessing))
        Seq(ScheduledEvent(ProcessedEvent(order), Option(this), receivers, now + distribution.draw()))
      } else {
        state = state.apply(NodeStateDiff(now, toQueue, Set(), List(), fromProcessing))
        Seq()
      }
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimator)

  override def warnings: Map[Monitor, String] = monitors.mapValues(_.warnings)
}

trait Order

trait Event

case object StartEvent extends Event

case object EndEvent extends Event

case class GenerateEvent(order: Order) extends Event

case class EnterEvent(order: Order) extends Event

case class ProcessedEvent(order: Order) extends Event

case class ScheduledEvent(event: Event, sender: Option[Entity], receivers: List[Entity], time: Double)

case class SimulatorState(next: ScheduledEvent, events: mutable.PriorityQueue[ScheduledEvent]) {
  def isStop: Boolean = next.event match {
    case EndEvent => true
    case _ => false
  }
}

case class Simulator(entities: List[Entity], sources: List[Entity], args: SimulatorArgs) {
  def simulate(): Try[Result] = Try {
    var state = init(entities, sources, args)
    while(!state.isStop) {
      state = triggerNext(state)
    }
    entities.foldLeft(Result(Map(), Map()))((lhs, rhs) => {
      lhs.copy(lhs.results ++ rhs.results, lhs.warnings ++ rhs.warnings)
    })
  }

  def triggerNext(state: SimulatorState): SimulatorState = {
    state.events.enqueue(state.next.receivers.flatMap(_.receive(state.next)): _*)
    if (state.events.isEmpty)
      SimulatorState(ScheduledEvent(EndEvent, Option.empty, List(), state.next.time), state.events)
    else {
      val next = state.events.dequeue
      SimulatorState(next, state.events)
    }
  }

  def init(entities: List[Entity], sources: List[Entity], simulatorArgs: SimulatorArgs): SimulatorState = {
    val queue = mutable.PriorityQueue.newBuilder[ScheduledEvent](Ordering.by(_.time))
    queue.enqueue(ScheduledEvent(EndEvent, Option.empty, List(), simulatorArgs.stopAt))
    SimulatorState(ScheduledEvent(StartEvent, Option.empty, sources, 0), queue)
  }
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

object Simulator {
  def networkEstimatorFactory(monitor: Monitor): EstimationAppender = monitor match {
    case SojournMonitor(_) => SojournEstimationAppender(monitor, ArrayBuffer(), mutable.Map())
  }
  def apply(network: Network, args: SimulatorArgs): Simulator = {
    val entities: List[Entity] = network.generators.flatMap(orderStream => {
      orderStream.trajectory match {
        case nt: NetworkTopology =>
          val nodeEntities = nt.services.map(pair => pair._1 -> NodeEntity(pair._2, Map(), NodeState(List(), pair._1.numUnits, List())))
          val monitors = network.monitors.map(monitor => monitor -> networkEstimatorFactory(monitor)).toMap
          val networkEntity = NetworkEntity(nt, monitors, NetworkState(nodeEntities))
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
