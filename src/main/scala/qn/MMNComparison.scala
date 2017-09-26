package qn

import breeze.plot._
import breeze.stats.distributions.DiscreteDistr
import galileo.environment.Environment
import galileo.expr._
import qn.chart.DrawPlot
import qn.distribution.Distribution
import qn.sim.network.CombinedNodeQuery
import qn.sim.network.estimator.{BacklogEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.solver.{DefaultQuerySet, ProductFormSolver, ProductFormSolverArgs}

import scala.util.Try

object MMNComparison {

  def main(args: Array[String]): Unit = mmn

  private def mmn = {
    val serverName = "Server"
    val server = Resource(serverName, 3)
    val networkName = "MM3"
    val network = Network(networkName, Seq(server))
      .add(OrdersStream(networkName, Distribution.exp(0.8), NetworkGraph()
        .addService(server, Distribution.exp(0.3))
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

    val ts = 0.0 to 12.0 by 0.5
    val analyticX = for (i <- ts) yield serverSojournDist.probability(0, i)
    val simulatedX = for (i <- ts) yield Try(sampledSojourn.probability(0, i)).getOrElse(0.0)

    val netAnalyticX = for (i <- ts) yield netSojournDist.probability(0, i)
    val netSimulatedX = for (i <- ts) yield Try(sampledNetSojourn.probability(0, i)).getOrElse(0.0)


    val figure = Figure()
    val numberOfOrders = figure.subplot(0)
    val sojournPlot = figure.subplot(2, 1, 1)
    val netSojournPlot = figure.subplot(3, 1, 2)


    DrawPlot.ofNumberOfOrders(numberOfOrders, xs.map(_.toDouble), analyticY, simulatedY)
    DrawPlot.ofSojournTime(sojournPlot, ts, analyticX, simulatedX)
    DrawPlot.ofSojournTime(netSojournPlot, ts, netAnalyticX, netSimulatedX)

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
