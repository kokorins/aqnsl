package qn.sim

import org.scalactic.TolerantNumerics
import org.scalatest.{Matchers, PropSpec}
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.monitor.SojournEstimation

class SimulatorTest extends PropSpec with Matchers {
  implicit val   doubleEq = TolerantNumerics.tolerantDoubleEquality(0.01)
  property("mm1 simulation") {
    val result = Simulator(Models.mm1_08, SimulatorArgs(100.0)).simulate()
    assert(result.isSuccess)
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.map({ case SojournEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should ===(2 * 1 / (1 - 0.8))
    }
    })
  }
}
