package qn

import org.scalactic.TolerantNumerics
import org.scalatest._
import qn.distribution.LaplaceBasedDistribution
import qn.model.Models
import qn.solver.{DefaultQuerySet, ProductFormSolver, ProductFormSolverArgs}

class NetworkDefinitionSpec extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.001)

  property("MM1 model") {
    val query = new DefaultQuerySet()
    val result = ProductFormSolver(Models.mm1_08, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    val distr = sojournEstimation.get
    distr match {
        case distr: LaplaceBasedDistribution => distr.mean should ===(5.0)
        case _ => fail()
    }
  }

  property("MM1-MM1 model") {
    val query = new DefaultQuerySet()
    val result = ProductFormSolver(Models.mm1mm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map(_.mean should ===(2 * 1 / (1 - 0.8)))
  }

  property("MM1 or MM1") {
    val query = new DefaultQuerySet()

    val result = ProductFormSolver(Models.mm1ormm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map(_.mean should ===(1 / (1 - 0.8)))
  }

  property("MM1-MM1-MM1") {
    val query = new DefaultQuerySet()

    val result = ProductFormSolver(Models.mm1mm1mm1, ProductFormSolverArgs(query)).solve()
    assert(result.isSuccess)
    val sojournEstimation = query.networkSojourn
    sojournEstimation.map(_.mean should ===(3 / (1 - 0.8)))
  }
}
