package qn

import org.scalactic.TolerantNumerics
import org.scalatest._
import qn.distribution.{Distribution, LaplaceBasedDistribution}
import qn.monitor.{SojournEstimation, SojournMonitor, StationaryDistributionMonitor}
import qn.solver.ProductFormSolver

class NetworkDefinitionSpec extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.001)

  property("MM1 model") {
    val result = ProductFormSolver(mm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(networkSojourn)
    sojournEstimation.get match {
      case SojournEstimation(_, distr) => distr match {
        case distr: LaplaceBasedDistribution => distr.mean should ===(5.0)
        case _ => fail()
      }
      case _ => fail()
    }
  }

  property("MM1-MM1 model") {
    val result = ProductFormSolver(mm1mm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(networkSojourn)
    sojournEstimation.map({ case SojournEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should ===(2 * 1 / (1 - 0.8))
    }
    })
  }

  property("MM1 or MM1") {
    val result = ProductFormSolver(mm1ormm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(networkSojourn)
    sojournEstimation.map({ case SojournEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(1 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }

  property("MM1-MM1-MM1") {
    val result = ProductFormSolver(mm1mm1mm1).solve()
    assert(result.isSuccess)
    val monitors = result.get.results
    assert(monitors.nonEmpty)
    val sojournEstimation = monitors(networkSojourn)
    sojournEstimation.map({ case SojournEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution =>
        dist.mean should ===(3 / (1 - 0.8))
      case _ => fail()
    }
    case _ => fail()
    })
  }

  private val stationaryDistribution: StationaryDistributionMonitor = StationaryDistributionMonitor("Pi Monitor")
  private val node1: Resource = Resource("Service unit 1", 1).add(stationaryDistribution)
  private val node2: Resource = Resource("Service unit 2", 1).add(stationaryDistribution)
  private val node3: Resource = Resource("Service unit 3", 1).add(stationaryDistribution)
  private val networkSojourn: SojournMonitor = SojournMonitor("Network Sojourn")
  private val mm1 = Network("MM1")
    .add(node1)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, Resource.sink)
      .addService(node1, Distribution.exp(1.0))))
    .add(networkSojourn)

  private val mm1mm1 = Network("MM1->MM1")
    .add(node1)
    .add(node2) 
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, node2)
      .addTransition(node2, Resource.sink)
      .addService(node1, Distribution.exp(1.0))
      .addService(node2, Distribution.exp(1.0))))
    .add(networkSojourn)

  private val mm1ormm1 = Network("MM1 or MM1")
    .add(node1)
    .add(node2)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addShares(Resource.source, (node1, 0.5), (node2, 0.5))
      .addTransition(node1, Resource.sink)
      .addTransition(node2, Resource.sink)
      .addService(node1, Distribution.exp(.5))
      .addService(node2, Distribution.exp(.5))))
    .add(networkSojourn)

  private val mm1mm1mm1 = Network("MM1->MM1->MM1")
    .add(node1)
    .add(node2)
    .add(node3)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, node2)
      .addTransition(node2, node3)
      .addTransition(node3, Resource.sink)
      .addService(node1, Distribution.exp(2))
      .addService(node2, Distribution.exp(1))
      .addService(node3, Distribution.exp(3))))
    .add(networkSojourn)
}
