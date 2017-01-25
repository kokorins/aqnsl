package qn.sim

import breeze.stats.distributions.ApacheContinuousDistribution
import org.scalactic.TolerantNumerics
import org.scalatest.{Matchers, PropSpec}
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.monitor.ContinuousEstimation
import qn.sim.network.estimator.SojournEstimator

import scala.collection.mutable

class SimulatorTest extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.01)
  property("mm1 simulation") {
    val networkSojourn = SojournEstimator("Network")
    val result = Simulator(Models.mm1_08, SimulatorArgs(networkSojourn, Map(), 10.0)).simulate()
    result.isSuccess should be(true)
    networkSojourn.estimate.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should be(2 * 1 / (1 - 0.8))
      case dist: ApacheContinuousDistribution => dist.mean should be(2 * 1 / (1 - 0.8) +- 0.1)
      case _ => fail()
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
