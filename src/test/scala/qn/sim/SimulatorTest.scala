package qn.sim

import org.scalactic.TolerantNumerics
import org.scalatest.{Matchers, PropSpec}
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.monitor.SojournEstimation

import scala.collection.mutable

class SimulatorTest extends PropSpec with Matchers {
  implicit val   doubleEq = TolerantNumerics.tolerantDoubleEquality(0.01)
  property("mm1 simulation") {
    val result = Simulator(Models.mm1_08, SimulatorArgs(100.0)).simulate()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.map({ case SojournEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should ===(2 * 1 / (1 - 0.8))
    }
    })
  }

  property("Simulator state") {
    val others = mutable.PriorityQueue[ScheduledCommand]()
    others.enqueue(ScheduledCommand(StartSimulatorCommand, Option.empty, List.empty, 0.0),
      ScheduledCommand(StartSimulatorCommand, Option.empty, List.empty, 1.0),
      ScheduledCommand(EndSimulatorCommand, Option.empty, List.empty, 3.0),
      ScheduledCommand(StartSimulatorCommand, Option.empty, List.empty, 2.0))
    others.dequeue().time === 0.0
    others.dequeue().time === 1.0
    others.dequeue().time === 2.0
    others.dequeue().time === 3.0
  }
}
