package qn.sim.network

import org.scalatest.{FunSuite, Matchers}
import qn.distribution.Singular
import qn.sim._

class NodeEntityTest extends FunSuite with Matchers {
  val nodeId = ""
  val distr = Singular(1)
  val order = Order(1)
  val order2 = Order(2)

  test("Should process node entering events") {

    val command = EnterSimulatorCommand(order)
    val nodeEntity = NodeEntity(nodeId, distribution = distr)
    val send = nodeEntity.receive(ScheduledCommand(command, Option.empty, List(), 0.0))
    val expectedEvent = ScheduledCommand(ProcessedSimulatorCommand(order), Option(nodeEntity), List(nodeEntity), 1.0)
    send should contain(expectedEvent)
    nodeEntity.state.numSlots === 1
    nodeEntity.state.processing should have(size(1))
    nodeEntity.state.queue should be(empty)
  }

  test("Should process node entering events with queue") {
    val command = EnterSimulatorCommand(order)
    val command2 = EnterSimulatorCommand(order2)
    val nodeEntity = NodeEntity(nodeId, distribution = distr)
    nodeEntity.receive(ScheduledCommand(command, Option.empty, List(), 0.0))
    val send = nodeEntity.receive(ScheduledCommand(command2, Option.empty, List(), 0.0))
    send should be(empty)
    nodeEntity.state.processing should have(size(1))
    nodeEntity.state.queue should have(size(1))
  }

  test("Should process node processed events") {
    val nodeEntity = NodeEntity(nodeId, distribution = distr, state = NodeState(1, List(), List(order)))
    val send = nodeEntity.receive(ScheduledCommand(ProcessedSimulatorCommand(order), Option(nodeEntity), List(), 0.0))
    send should be(empty)
    nodeEntity.state.processing should be(empty)
    nodeEntity.state.queue should be(empty)
  }

  test("Should process node processed events with queue") {
    val nodeEntity = NodeEntity(nodeId, distribution = distr, state = NodeState(1, List(order2), List(order)))
    val send = nodeEntity.receive(ScheduledCommand(ProcessedSimulatorCommand(order), Option(nodeEntity), List(), 0.0))
    send should have(size(1))
    nodeEntity.state.processing should contain(order2)
    nodeEntity.state.queue should be(empty)
  }

}
