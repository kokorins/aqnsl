package qn.sim.network

import qn.distribution.Distribution
import qn.sim._
import qn.util.ImmutableBiMap
import qn.{NetworkGraph, Resource, Transition}

import scala.collection.mutable

case class NetworkStateEvent(at: Double, networkIn: Set[Order], networkOut: Set[Order])

case class NetworkState(orders: Set[Order]) {
  def apply(networkStateEvent: NetworkStateEvent): NetworkState = {
    NetworkState(orders.diff(networkStateEvent.networkOut) ++ networkStateEvent.networkIn)
  }
}

trait NetworkQuery {
  def append(event: NetworkStateEvent): Unit
}

case object EmptyNetworkQuery extends NetworkQuery {
  override def append(event: NetworkStateEvent): Unit = {}
}

case class NetworkLogger(events: mutable.ArrayBuffer[NetworkStateEvent]) extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = events.append(networkStateEvent)
}

case class NetworkStructure(nodeEntities: ImmutableBiMap[Resource, NodeEntity])

case class NetworkEntity(transitions: Map[Resource, Set[Transition]], structure: NetworkStructure,
                         networkQuery: NetworkQuery = EmptyNetworkQuery, var state: NetworkState = NetworkState(Set()))
  extends Entity {
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
      val fromResource:Resource = (structure.nodeEntities.inverse)(node) // intellij bug, without brackets doesnt recognize implicit conversion
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

object NetworkEntity {
  def fromGraph(networkGraph: NetworkGraph, structure: NetworkStructure,
                networkQuery: NetworkQuery = EmptyNetworkQuery,
                state: NetworkState = NetworkState(Set())): NetworkEntity = new NetworkEntity(
    networkGraph.transitions.groupBy(_.from), structure, networkQuery, state)

}

