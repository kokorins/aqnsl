package qn

import breeze.stats._
import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}
import qn.sim.network.CombinedNetworkQuery
import qn.sim.network.estimator.{SojournEstimator, SojournMomentsEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.solver._

object WarehouseModel {

  def main(args: Array[String]): Unit = {
    val networkGraph = warehouse()
    println(DotTransformer.toDot(networkGraph, new DotConfig() {
      override val showSource = true
    }))

    val network = Network("Warehouse Model", generators = List(OrdersStream("", Distribution.exp(0.8), networkGraph)))
      .add(slowPick).add(slowSort).add(slowPack).add(fastPick).add(fastPack)

    val networkSojourn = SojournEstimator("Sojourn Time Sample")
    val networkMomSojourn = SojournMomentsEstimator("Sojourn Time Sample")
    val networkQuery = CombinedNetworkQuery(networkSojourn, networkMomSojourn)
    val simulator = Simulator(network, SimulatorArgs(100, networkQuery))
    val prodResults = new DefaultQuerySet()
    ProductFormSolver(network, ProductFormSolverArgs(prodResults)).solve()
    println(simulator.simulate())
    println(
      s"${networkSojourn.name} mean(var): ${mean(networkSojourn.sample)}(${variance(networkSojourn.sample)})")
    println(
      s"${networkMomSojourn.name} mean(var): ${networkMomSojourn.adder.mean}(${networkMomSojourn.adder.v})")
    println(s"${fastPick.name} stationary distribution: ${prodResults.nodesStationary(fastPick)}")
    println(s"Network sojourn mean: ${prodResults.networkSojourn.map(_.mean)}")
  }

  private val slowPickLaneName = "Slow Pick"
  private val slowSortLaneName = "Slow Sort"
  val slowPackLaneName = "Slow Pack"
  val fastPickLaneName = "Fast Pick"
  val fastPackName = "Fast Pack"
  val slowPick = Resource(slowPickLaneName, 20)
  val slowSort = Resource(slowSortLaneName, 8)
  val slowPack = Resource(slowPackLaneName, 8)
  val fastPick = Resource(fastPickLaneName, 8)
  val fastPack = Resource(fastPackName, 4)


  def slowChainNetwork(): NetworkGraph = {
    val networkName = "Slow Chain Network"
    NetworkGraph(networkName)
      .addService(slowPick, Distribution.exp(1))
      .addService(slowSort, Distribution.exp(1))
      .addService(slowPack, Distribution.exp(1))
      .addTransition(Resource.source, slowPick)
      .addTransition(slowPick, slowSort)
      .addTransition(slowSort, slowPack)
      .addTransition(slowPack, Resource.sink)
  }

  def warehouse(): NetworkGraph = {
    NetworkGraph("Warehouse model")
      .addService(slowPick, Distribution.exp(1))
      .addService(fastPick, Distribution.exp(1))
      .addService(slowSort, Distribution.exp(1))
      .addService(slowPack, Distribution.exp(1))
      .addService(fastPack, Distribution.exp(1))
      .addShares(Resource.source, (slowPick, 0.9), (fastPick, 0.1))
      .addShares(slowPick, (slowSort, 0.6), (fastPick, 0.4))
      .addTransition(slowSort, slowPack)
      .addShares(fastPick, (fastPack, 0.9), (slowPick, 0.1))
      .addTransition(fastPack, Resource.sink)
      .addTransition(slowPack, Resource.sink)
  }
}
