package qn

import org.scalactic.TolerantNumerics
import org.scalatest._
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.monitor.ContinuousEstimation
import qn.solver.ProductFormSolver

class NetworkDefinitionSpec extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.001)

  property("MM1 model") {
    val result = ProductFormSolver(Models.mm1_08).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.get match {
      case ContinuousEstimation(_, distr) => distr match {
        case distr: LaplaceBasedDistribution => distr.mean should ===(5.0)
        case _ => fail()
      }
      case _ => fail()
    }
  }

  property("MM1-MM1 model") {
    val result = ProductFormSolver(Models.mm1mm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should ===(2 * 1 / (1 - 0.8))
    }
    })
  }

  property("MM1 or MM1") {
    val result = ProductFormSolver(Models.mm1ormm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(1 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }

  property("MM1-MM1-MM1") {
    val result = ProductFormSolver(Models.mm1mm1mm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(Models.networkSojourn)
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(3 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }
}
