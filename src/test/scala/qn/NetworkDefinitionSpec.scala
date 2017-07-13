package qn

import org.scalactic.TolerantNumerics
import org.scalatest._
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.monitor.ContinuousEstimation
import qn.solver.{DefaultQuerySet, ProductFormSolver, ProductFormSolverArgs}

class NetworkDefinitionSpec extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.001)

  property("MM1 model") {
    val query = new DefaultQuerySet()
    val result = ProductFormSolver(Models.mm1_08, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.get match {
      case ContinuousEstimation(_, distr) => distr match {
        case distr: LaplaceBasedDistribution => distr.mean should ===(5.0)
        case _ => fail()
      }
      case _ => fail()
    }
  }

  property("MM1-MM1 model") {
    val query = new DefaultQuerySet()
    val result = ProductFormSolver(Models.mm1mm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should ===(2 * 1 / (1 - 0.8))
    }
    })
  }

  property("MM1 or MM1") {
    val query = new DefaultQuerySet()

    val result = ProductFormSolver(Models.mm1ormm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(1 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }

  property("MM1-MM1-MM1") {
    val query = new DefaultQuerySet()

    val result = ProductFormSolver(Models.mm1mm1mm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(3 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }
}
