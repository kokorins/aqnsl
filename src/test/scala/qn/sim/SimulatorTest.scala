package qn.sim

import breeze.stats.distributions.{ApacheContinuousDistribution, ApacheDiscreteDistribution}
import org.scalactic.TolerantNumerics
import org.scalatest.{Matchers, PropSpec}
import qn.Resource.{sink, source}
import qn.distribution.{Distribution, LaplaceBasedDistribution, Singular}
import qn.model.Models
import qn.monitor.{ContinuousEstimation, DiscreteEstimation}
import qn.sim.network.estimator.{BacklogEstimator, ProcessedEstimator, SojournEstimator}
import qn.{Network, NetworkTopology, OrdersStream, Resource}

import scala.collection.mutable

class SimulatorTest extends PropSpec with Matchers {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.01)
  property("mm1 simulation") {
    val networkSojourn = SojournEstimator("Network")
    val result = Simulator(Models.mm1_08, SimulatorArgs(10.0, networkSojourn)).simulate()
    result.isSuccess should be(true)
    networkSojourn.estimate.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: LaplaceBasedDistribution => dist.mean should be(2 * 1 / (1 - 0.8))
      case dist: ApacheContinuousDistribution => dist.mean should be(2 * 1 / (1 - 0.8) +- 0.1)
      case _ => fail()
    }
    }).get
  }

  property("dd1 simulation with checks") {
    val processed = ProcessedEstimator("Network")
    val result = Simulator(Models.dd1, SimulatorArgs(10.5, processed)).simulate()
    result.isSuccess should be(true)
    processed.estimate.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: Singular => dist.value should be(10.0 +- 0.1)
      case _ => fail()
    }
    }).get
  }

  property("Test from worksheet") {
    val server: Resource = Resource("Server", 1)

    val networkName = "MM1"
    val rate = 0.8
    val mm1 = Network(networkName)
              .add(server)
              .add(OrdersStream(networkName, Distribution.exp(rate),
                NetworkTopology()
                .addTransition(source, server)
                .addTransition(server, sink)
                .addService(server, Distribution.exp(1.0))
              ))

    val networkProcessed = ProcessedEstimator(networkName)
    val nodeBacklog = BacklogEstimator(server)
    val stopAt = 1000.0
    val sim = Simulator(mm1, SimulatorArgs(stopAt, networkProcessed, Map(server -> nodeBacklog)))
    sim.simulate()

    nodeBacklog.estimate.map({ case DiscreteEstimation(_, distr) => distr match {
      case dist: ApacheDiscreteDistribution => dist.probabilityOf(0) should be (0.2 +- 0.1)
      case _ => fail()
    }
    }).get
    networkProcessed.estimate.map({ case ContinuousEstimation(_, distr) => distr match {
      case dist: Singular => dist.value should be(stopAt * rate +-.1 * (stopAt * rate))
      case _ => fail()
    }
    }).get
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
