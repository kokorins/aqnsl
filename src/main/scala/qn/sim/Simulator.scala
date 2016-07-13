package qn.sim

import qn.Network
import qn.solver.Result

import scala.collection.mutable
import scala.util.Try

case class SimulatorArgs()

trait Event {}

case class ScheduledEvent[Event](event:Event, time:Double )

case class SimulatorState[Event](var now:Double, events:mutable.PriorityQueue[ScheduledEvent[Event]]) {
  def enqueue(event:ScheduledEvent[Event]) = {
    events.enqueue(event)
  }
  def deque() = {
    val event = events.dequeue
    now = event.time
    event
  }

  def triggerNext() = ???
  def isStop:Boolean = ???
  def result:Result = ???
}

case class Simulator(network: Network) {
  def simulate(args:SimulatorArgs):Try[Result] = Try {
    val state = init(args, network)
    while(!state.isStop) {
      state.triggerNext()
    }
    state.result
  }

  def init(simulatorArgs: SimulatorArgs, network: Network):SimulatorState[Event] = ???

}
