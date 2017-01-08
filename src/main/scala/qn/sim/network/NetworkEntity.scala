package qn.sim.network

import qn.distribution.Distribution
import qn.monitor.{Estimation, Monitor}
import qn.sim._
import qn.{NetworkTopology, Resource}

import scala.collection.mutable
import scala.util.Try

case class NetworkStateEvent(at: Double, networkIn: Set[Order], networkOut: Set[Order])

case class NetworkState(orders: Set[Order]) {
  def apply(networkStateEvent: NetworkStateEvent): NetworkState = {
    NetworkState(orders.diff(networkStateEvent.networkOut) ++ networkStateEvent.networkIn)
  }
}

trait NetworkQuery {
  def append(networkStateEvent: NetworkStateEvent): Unit = {}
}

case class NetworkLogger(events: mutable.ArrayBuffer[NetworkStateEvent]) extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = events.append(networkStateEvent)
}

case class NetworkStructure(nodeEntities: Map[Resource, NodeEntity])

case class NetworkEntity(networkTopology: NetworkTopology, monitors: Map[Monitor, EstimationAppender] = Map(), structure: NetworkStructure, var state: NetworkState = NetworkState(Set()), networkQuery: NetworkQuery = new NetworkQuery {}) extends ResultEntity {
  override def receive(networkEvent: ScheduledCommand): Seq[ScheduledCommand] = networkEvent match {
    case ScheduledCommand(GenerateSimulatorCommand(order), _, _, at) =>
      val sources = networkTopology.transitions.filter(_.from == Resource.source).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = structure.nodeEntities(sources(dist.draw()).to)
      val event = NetworkStateEvent(at, Set(order), Set())
      state = state.apply(event)
      networkQuery.append(event)
      Seq(ScheduledCommand(EnterSimulatorCommand(order), Option(this), List(to), at))
    case ScheduledCommand(ProcessedSimulatorCommand(order), node, _, at) =>
      val sources = networkTopology.transitions.filter(_.from == node.get).toList
      val dist = Distribution.multi(sources.map(_.share): _*)
      val toResource = sources(dist.draw()).to
      if (toResource == Resource.sink) {
        val event = NetworkStateEvent(at, Set(), Set(order))
        state = state.apply(event)
        networkQuery.append(event)
        Seq()
      } else {
        val to = structure.nodeEntities(toResource)
        val event = NetworkStateEvent(at, Set(), Set())
        state = state.apply(event)
        networkQuery.append(event)
        Seq(ScheduledCommand(EnterSimulatorCommand(order), Option(this), List(to), at))
      }
  }

  override def results: Map[Monitor, Try[Estimation]] = monitors.mapValues(_.estimate)
}

