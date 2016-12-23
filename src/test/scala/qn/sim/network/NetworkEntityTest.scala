package qn.sim.network

import org.scalatest.{FunSuite, Matchers}
import qn.distribution.Singular
import qn.sim.{GenerateSimulatorCommand, Order, ScheduledCommand}
import qn.{NetworkTopology, Resource}

class NetworkEntityTest extends FunSuite with Matchers {
  val distr = Singular(1)
  val order = Order(1)
  test("Should handle arrived order") {
    val node = Resource("Node", 1)
    val topology = NetworkTopology("Topology")
                   .addService(node, distr)
                   .addTransition(Resource.source, node)
                   .addTransition(node, Resource.sink)
    val structure = NetworkStructure(Map(node -> NodeEntity(distr)))
    val network = NetworkEntity(topology, state = NetworkState(Set()), structure = structure)
    val send = network.receive(ScheduledCommand(GenerateSimulatorCommand(order), Option.empty, List(network), 0.0))
  }
}