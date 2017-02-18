package qn

import breeze.plot._
import breeze.stats.distributions.{ContinuousDistr, DiscreteDistr}
import org.jfree.chart.axis.NumberTickUnit
import qn.distribution.Distribution
import qn.monitor._
import qn.sim.network.CombinedNodeQuery
import qn.sim.network.estimator.{BacklogEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.solver.ProductFormSolver

object MM1Comparison {
  val Gray = "109, 109, 109"
  val LightGray = "191, 191, 191"
  val Orange = "255, 105, 0"
  val LightOrange = "254, 166, 101"
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
    val res = Simulator(network, SimulatorArgs(10009, sojourn, Map(server -> CombinedNodeQuery(serverBacklog, serverSojourn)))).simulate()

    val serverBacklogDist = solution.get.results(serverBacklogMonitor).get match {
      case StationaryDistributionEstimation(_, sbd) => sbd
    }
    val serverSojournDist = solution.get.results(serverSojournMonitor).get match {
      case ContinuousEstimation(_, continuousDistr) => continuousDistr
    }
    val sampledBacklog: DiscreteDistr[Int] = serverBacklog.estimate.get match {
      case DiscreteEstimation(_, sampleBacklog) => sampleBacklog
    }
    val sampledSojourn = serverSojourn.estimate.get match {
      case ContinuousEstimation(_, continuousDistr) => continuousDistr
    }
    val xs = 0 to 15
    val analyticY = for (i <- xs) yield serverBacklogDist.probabilityOf(i)
    val simulatedY = for (i <- xs) yield sampledBacklog.probabilityOf(i)
    val figure = Figure("MM1 Queueing System")
    val numberOfOrders = figure.subplot(0)
    numberOfOrders += plot(xs.map(_.toDouble), analyticY, name = "Analytical", colorcode = Gray)
    numberOfOrders += plot(xs.map(_.toDouble), simulatedY, name = "Simulated", colorcode = Orange)
    numberOfOrders.title = "Number of Orders"
    numberOfOrders.xlabel = "Number of Orders"
    numberOfOrders.ylabel = "Estimated Probability"
    numberOfOrders.legend = true
    numberOfOrders.yaxis.setTickUnit(new NumberTickUnit(0.02))
    numberOfOrders.yaxis.setAutoRangeIncludesZero(true)

    val ts = 0.0 to 13.0 by 0.5
    val analyticX = for (i <- ts) yield serverSojournDist.probability(0, i.toDouble)
    val simulatedX = for (i <- ts) yield sampledSojourn.probability(0, i.toDouble)

    val sojournPlot = figure.subplot(2, 1, 1)
    sojournPlot += plot(ts, analyticX, name = "Analytical", colorcode = Gray)
    sojournPlot += plot(ts, simulatedX, name = "Simulated", colorcode = Orange)

    sojournPlot.title = "Sojourn Time"
    sojournPlot.xlabel = "Time Units"
    sojournPlot.ylabel = "Estimated Probability"
    sojournPlot.legend = true
    sojournPlot.yaxis.setTickUnit(new NumberTickUnit(0.05))
    sojournPlot.yaxis.setAutoRangeIncludesZero(true)
  }
}
