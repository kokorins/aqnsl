import breeze.stats.distributions.Exponential
import qn.Resource._
import qn.distribution.Distribution
import qn.monitor.{SojournMonitor, StationaryDistributionMonitor}
import qn.solver.Solver
import qn.{NetworkTopology, Transition, _}

val server: Resource = Resource("Server", 1)
val stationaryDistNetwork = StationaryDistributionMonitor("Stationary Distribution Network")
val sojournMonitor = SojournMonitor("Sojourn Nodes")
val mm1 = Network("MM1")
  .add(server)
  .add(OrdersStream("Orders", Distribution.exp(0.8), NetworkTopology()
    .add(Transition(source, server, 1.0))
    .add(Transition(server, sink, 1.0))))
  .add(stationaryDistNetwork)
  .add(sojournMonitor)
val resultTry = Solver.prodForm(mm1)
val result = resultTry.get
val downtime = result.results(stationaryDistNetwork).map({
  case StationaryDistributionEstimation(_, discreteDistribution) => discreteDistribution(0)
})
val sojourn = result.results(sojournMonitor).map({
  case SojournEstimation(_, distribution) => distribution match {
    case exp: Exponential => exp.mean
  }
})