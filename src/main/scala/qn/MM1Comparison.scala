package qn

import qn.distribution.Distribution
import qn.monitor.{SojournMonitor, StationaryDistributionEstimation, StationaryDistributionMonitor}
import qn.sim.network.CombinedNodeQuery
import qn.sim.network.estimator.{BacklogEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.solver.ProductFormSolver

object MM1Comparison {
  def main(args: Array[String]): Unit = {
    val serverName = "Server"
    val serverSojournMonitor = SojournMonitor(serverName)
    val serverBacklogMonitor = StationaryDistributionMonitor(serverName)
    val server = Resource(serverName, 1)
                 .add(serverSojournMonitor)
                 .add(serverBacklogMonitor)
    val networkName = "MM1"
    val networkSojournMonitor = SojournMonitor(networkName)
    val network = Network(networkName, Seq(server), monitors = List(networkSojournMonitor))
                  .add(OrdersStream(networkName, Distribution.exp(0.8), NetworkTopology()
                                                                        .addService(server, Distribution.exp(1.0))
                                                                        .addTransition(Resource.source, server)
                                                                        .addTransition(server, Resource.sink)))
    val solution = ProductFormSolver(network).solve()
    val sojourn = SojournEstimator(networkName)
    val serverSojourn = SojournEstimator(serverName)
    val serverBacklog = BacklogEstimator(server)
    Simulator(network, SimulatorArgs(1000, sojourn, Map(server -> CombinedNodeQuery(serverBacklog, serverSojourn)))).simulate()

    val serverBacklogDist = solution.get.results(serverBacklogMonitor).get match {
      case StationaryDistributionEstimation(_, serverBacklogDist) => serverBacklogDist
    }
    println(serverBacklogDist)
    println(serverBacklog.estimate.get)
  }
}
