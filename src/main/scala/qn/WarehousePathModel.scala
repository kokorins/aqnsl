package qn

import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}

object WarehousePathModel {

  def main(args: Array[String]): Unit = {
    val network = warehouse()
    println(DotTransformer.toDot(network, new DotConfig() {
      override val showSource = true
    }))

    //    val network = Network("Warehouse Model", generators = List(OrdersStream("", Distribution.exp(0.8),
    // networkGraph)))
    //      .add(slowPick).add(slowSort).add(slowPack).add(fastPick).add(fastPack)

    //    val networkSojourn = SojournEstimator("Sojourn Time Sample")
    //    val networkMomSojourn = SojournMomentsEstimator("Sojourn Time Sample")
    //    val networkQuery = CombinedNetworkQuery(networkSojourn, networkMomSojourn)
    //    val simulator = Simulator(network, SimulatorArgs(100, networkQuery))
    //    val prodResults = new DefaultQuerySet()
    //    ProductFormSolver(network, ProductFormSolverArgs(prodResults)).solve()
    //    println(simulator.simulate())
    //    println(
    //      s"${networkSojourn.name} mean(var): ${mean(networkSojourn.sample)}(${variance(networkSojourn.sample)})")
    //    println(
    //      s"${networkMomSojourn.name} mean(var): ${networkMomSojourn.adder.mean}(${networkMomSojourn.adder.v})")
    //    println(s"${fastPick.name} stationary distribution: ${prodResults.nodesStationary(fastPick)}")
    //    println(s"Network sojourn mean: ${prodResults.networkSojourn.map(_.mean)}")
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
