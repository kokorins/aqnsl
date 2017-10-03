package qn.examples

import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}
import qn.sim.network.CombinedNetworkQuery
import qn.sim.network.estimator.{SojournEstimator, SojournMomentsEstimator}
import qn.sim.{Simulator, SimulatorArgs}
import qn.{Network, NetworkGraph, OrdersStream, Resource}
import breeze.stats._

object WarehousePathModel {

  def main(args: Array[String]): Unit = {
    val network = warehouse()
    println(DotTransformer.toDot(network, new DotConfig() {
      override val showSource = true
    }))

    val networkSojourn = SojournEstimator("Sojourn Time Sample")
    val networkMomSojourn = SojournMomentsEstimator("Sojourn Time Sample")
    val networkQuery = CombinedNetworkQuery(networkSojourn, networkMomSojourn)

    val simulator = Simulator(network, SimulatorArgs(100, networkQuery))

    println(simulator.simulate())

    println(s"${networkSojourn.name} mean(var): ${mean(networkSojourn.sample)}(${variance(networkSojourn.sample)})")
    println(s"${networkMomSojourn.name} mean(var): ${networkMomSojourn.adder.mean}(${networkMomSojourn.adder.v})")
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

  def warehouse(): Network = {
    val pureSlowPath = NetworkGraph("Pure Slow")
      .addService(slowPick, Distribution.exp(1))
      .addService(slowSort, Distribution.exp(1))
      .addService(slowPack, Distribution.exp(1))
      .addTransition(Resource.source, slowPick)
      .addTransition(slowPick, slowSort)
      .addTransition(slowSort, slowPack)
      .addTransition(slowPack, Resource.sink)

    val pureFastPath = NetworkGraph("Pure Fast")
      .addService(fastPick, Distribution.exp(1))
      .addService(fastPack, Distribution.exp(1))
      .addTransition(Resource.source, fastPick)
      .addTransition(fastPick, fastPack)
      .addTransition(fastPack, Resource.sink)

    Network("Warehouse Model",
      generators = List(
        OrdersStream("Pure Slow", Distribution.exp(0.4), pureSlowPath),
        OrdersStream("Pure Fast", Distribution.exp(0.4), pureFastPath)
      ))
      .add(slowPick).add(slowSort).add(slowPack).add(fastPick).add(fastPack)
  }
}
