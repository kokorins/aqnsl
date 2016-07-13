package qn

import breeze.stats.distributions.Geometric
import org.specs2.matcher.{MatcherMacros, ThrownMessages}
import org.specs2.mutable._
import org.specs2.specification.Scope
import qn.distribution.Distribution
import qn.monitor.{StationaryDistributionEstimation, StationaryDistributionMonitor}
import qn.solver.Solver

import scala.language.experimental.macros

class NetworkDefinitionSpec extends Specification with MatcherMacros with ThrownMessages {


  "Define Indicators: Stationary Distribution for M/M/1" should {
    "be a geometric distribution with parameter = interArrival / service rate" in new mm1 {
      val result = Solver.prodForm(model)
      result should beSuccessfulTry.which(monitors => {
        val stationaryDistribution = monitors.results(node.monitors.head)
        stationaryDistribution should beSuccessfulTry.which {
          case StationaryDistributionEstimation(_, dist) => dist match {
            case Geometric(p) => p should beCloseTo(0.8 +/- 0.01)
            case _ => fail("Improper distribution")
          }
          case _ => fail("Improper distribution")
        }
      })
    }
  }

  trait mm1 extends Scope {
    import Resource._

    val node: Resource = Resource("Service unit", 1).add(StationaryDistributionMonitor("Pi Monitor"))
    val model = Network("MM1")
      .add(node)
      .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
        .addTransition(source, node)
        .addTransition(node, sink)
        .addService(node, Distribution.exp(1.0))))
  }
}
