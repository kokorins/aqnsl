package qn

import org.specs2._
import org.specs2.matcher.{MatchResult, MatcherMacros}

import scala.util.Try

class NetworkDefinitionSpec extends mutable.Specification with MatcherMacros {
  "Define M/M/1 model" >> {
    val server: Resource = Resource("Server", 1)
    val mm1 = Network("MM1")
      .add(server)
      .add(OrdersStream("Orders", Distribution.exp(0.8), Chain()
        .add(Seize(server, 1).add(Timeout(Distribution.exp(1.0))))
      ))
    mm1.resources should contain(server)
  }

  "Define Chain: M/M/1 -> M/M/1" >> {
    val server1: Resource = Resource("Server1", 1)
    val server2: Resource = Resource("Server2", 1)
    val mm1 = Network("MM1 -> MM1")
      .add(server1)
      .add(server2)
      .add(OrdersStream("Orders", Distribution.exp(0.8), Chain()
        .add(Seize(server1, 1).add(Timeout(Distribution.exp(1.0))))
        .add(Seize(server2, 1).add(Timeout(Distribution.exp(1.0))))
      ))
    mm1.resources should contain(server1, server2)
  }

  "Define Alternatives: (0.5:M/M/1 + 0.5:M/M/1)" >> {
    val server1: Resource = Resource("Server1", 1)
    val server2: Resource = Resource("Server2", 1)
    val mm1 = Network("(0.5:M/M/1 + 0.5:M/M/1)")
      .add(server1)
      .add(server2)
      .add(OrdersStream("Orders", Distribution.exp(0.8), Chain()
        .add(Branch(RandomShares(Set(
          (0.5, Chain("").seize(server1, 1, Distribution.exp(1.0))),
          (0.5, Chain("").seize(server2, 1, Distribution.exp(1.0)))
        ))))))
    mm1.resources should contain(server1, server2)
  }

  "Define Indicators: Mean Sojourn time for M/M/1" >> {
    import Resource._
    val server: Resource = Resource("Server", 1)
    val sojournNetwork: Sojourn = Sojourn("Network")
    val stationaryDistNetwork: StationaryDistribution = StationaryDistribution("Network")
    val mm1 = Network("MM1")
      .add(server)
      .add(OrdersStream("Orders", Distribution.exp(0.8), NetworkTopology()
        .add(Transition(source, server, 1.0))
        .add(Transition(server, sink, 1.0))))
      .add(sojournNetwork)
      .add(stationaryDistNetwork)
    val resultTry = Solver.prodForm(mm1)
    val result = resultTry.get
    val meanSojournTime: Try[Double] = result.results(sojournNetwork).map(dist => Statistics.mean(dist))
    meanSojournTime.toOption.fold(failure.asInstanceOf[MatchResult[Double]])(_ should beCloseTo(4.2, 0.1))
  }
}
