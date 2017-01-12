package qn.sim.network

import qn.distribution.Distribution
import qn.sim._
import qn.util.ImmutableBiMap
import qn.{NetworkTopology, Resource, Transition}

import scala.collection.mutable

case class NetworkStateEvent(at: Double, networkIn: Set[Order], networkOut: Set[Order])

case class NetworkState(orders: Set[Order]) {
  def apply(networkStateEvent: NetworkStateEvent): NetworkState = {
    NetworkState(orders.diff(networkStateEvent.networkOut) ++ networkStateEvent.networkIn)
  }
}

trait NetworkQuery {
  def append(networkStateEvent: NetworkStateEvent): Unit
}

case object EmptyQuery extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = {}
}

case class NetworkLogger(events: mutable.ArrayBuffer[NetworkStateEvent]) extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = events.append(networkStateEvent)
}

case class NetworkStructure(nodeEntities: ImmutableBiMap[Resource, NodeEntity])

case class NetworkEntity(networkTopology: NetworkTopology, structure: NetworkStructure, networkQuery: NetworkQuery = EmptyQuery, var state: NetworkState = NetworkState(Set())) extends Entity {
  val transitions: Map[Resource, Set[Transition]] = networkTopology.transitions.groupBy(_.from)

  override def receive(networkEvent: ScheduledCommand): Seq[ScheduledCommand] = networkEvent match {
    case ScheduledCommand(GenerateSimulatorCommand(order), _, _, at) =>
      val sources = transitions(Resource.source).toSeq
      val dist = Distribution.multi(sources.map(_.share): _*)
      val to = structure.nodeEntities(sources(dist.draw()).to)
      val event = NetworkStateEvent(at, Set(order), Set())
      state = state.apply(event)
      networkQuery.append(event)
      Seq(ScheduledCommand(EnterSimulatorCommand(order), Option(this), List(to), at))
    case ScheduledCommand(ProcessedSimulatorCommand(order), Some(node:NodeEntity), _, at) =>
      val fromResource:Resource = (structure.nodeEntities.inverse)(node) // intellij bug, withour brackets doesnt recognize implicit conversion
      val nextNodes = transitions(fromResource).toSeq
      val dist = Distribution.multi(nextNodes.map(_.share): _*)
      val toResource = nextNodes(dist.draw()).to
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
}

