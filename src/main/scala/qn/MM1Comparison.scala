package qn

import breeze.plot._
import breeze.stats.distributions.DiscreteDistr
import galileo.environment.Environment
import galileo.expr._
import org.jfree.chart.axis.NumberTickUnit
import qn.distribution.Distribution
import qn.sim.network.CombinedNodeQuery
import qn.sim.network.estimator.{BacklogEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.solver.{DefaultQuerySet, ProductFormSolver, ProductFormSolverArgs}

import scala.util.Try

object MM1Comparison {

  def main(args: Array[String]): Unit = mm1

  private def mm1 = {
    val serverName = "Server"
    val server = Resource(serverName, 1)
    val networkName = "MM1"
    val network = Network(networkName, Seq(server))
      .add(OrdersStream(networkName, Distribution.exp(0.8), NetworkGraph()
        .addService(server, Distribution.exp(1.0))
        .addTransition(Resource.source, server)
        .addTransition(server, Resource.sink)))
    val query = new DefaultQuerySet()
    ProductFormSolver(network, ProductFormSolverArgs(query)).solve()
    val sojourn = SojournEstimator(networkName)
    val serverSojourn = SojournEstimator(serverName)
    val serverBacklog = BacklogEstimator(server)
    Simulator(network, SimulatorArgs(100009, sojourn, Map(server -> CombinedNodeQuery(serverBacklog, serverSojourn))))
      .simulate()

    val serverBacklogDist = query.nodesStationary(server)

    val serverSojournDist = query.nodesSojourn(server)

    val netSojournDist = query.networkSojourn.get

    val sampledBacklog: DiscreteDistr[Int] = serverBacklog.estimate.get
    val sampledSojourn = serverSojourn.estimate.get
    val sampledNetSojourn = sojourn.estimate.get
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

    val ts = 0.0 to 12.0 by 0.5
    val analyticX = for (i <- ts) yield serverSojournDist.probability(0, i)
    val simulatedX = for (i <- ts) yield Try(sampledSojourn.probability(0, i)).getOrElse(0.0)

    val sojournPlot = figure.subplot(2, 1, 1)
    sojournPlot += plot(ts, analyticX, name = "Analytical", colorcode = Gray)
    sojournPlot += plot(ts, simulatedX, name = "Simulated", colorcode = Orange)

    sojournPlot.title = "Sojourn Time"
    sojournPlot.xlabel = "Time Units"
    sojournPlot.ylabel = "Estimated Probability"
    sojournPlot.legend = true
    sojournPlot.yaxis.setTickUnit(new NumberTickUnit(0.05))
    sojournPlot.yaxis.setAutoRangeIncludesZero(true)

    val netSojournPlot = figure.subplot(3, 1, 2)
    val netAnalyticX = for (i <- ts) yield netSojournDist.probability(0, i)
    val netSimulatedX = for (i <- ts) yield Try(sampledNetSojourn.probability(0, i)).getOrElse(0.0)

    println(netAnalyticX)
    netSojournPlot += plot(ts, netAnalyticX, name = "Analytical", colorcode = Gray)
    netSojournPlot += plot(ts, netSimulatedX, name = "Simulated", colorcode = Orange)

    println(1 / netSojournDist.mean, sampledNetSojourn.mean)
  }

  val Gray = "109, 109, 109"
  val LightGray = "191, 191, 191"
  val Orange = "255, 105, 0"
  val LightOrange = "254, 166, 101"

  private def calcAt(func: Expr, t: Double) = {
    val env = new Environment(Option.empty)
    env.set("t", Number(t))
    func.visit().visit(Option(env)).eval().doubleValue
  }

}
