package qn

import breeze.stats._
import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}
import qn.monitor._
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
    val pickDistr = NodeStationProductQuery(fastPick)
    val pickSojourn = NodeSojournProductQuery(fastPick)
    val tryNetworkSolution = ProductFormSolver(network,
      ProductFormSolverArgs(CombinedNetworkProductQuery(Seq(pickDistr, pickSojourn)))).solve()
    val networkSolution = tryNetworkSolution.get
    println(simulator.simulate())
    println(
      s"${networkSojourn.monitor.name} mean(var): ${mean(networkSojourn.sample)}(${variance(networkSojourn.sample)})")
    println(
      s"${networkMomSojourn.monitor.name} mean(var): ${networkMomSojourn.adder.mean}(${networkMomSojourn.adder.v})")
    println(s"Pick distribution: ${pickDistr.distr}")
    println(s"Pick sojourn: ${pickSojourn.distr}")
  }

  private val slowPickLaneName = "Slow Pick"
  private val bufferZoneName = "Buffer Zone"
  private val slowSortLaneName = "Slow Sort"
  val slowPackLaneName = "Slow Pack"
  val fastPickLaneName = "Fast Pick"
  val fastPackName = "Fast Pack"
  val slowPick = Resource(slowPickLaneName, 20).add(StationaryDistributionMonitor(slowPickLaneName))
    .add(SojournMonitor(slowPickLaneName))
  val slowSort = Resource(slowSortLaneName, 8).add(StationaryDistributionMonitor(slowSortLaneName))
    .add(SojournMonitor(slowSortLaneName))
  val slowPack = Resource(slowPackLaneName, 8).add(StationaryDistributionMonitor(slowPackLaneName))
    .add(SojournMonitor(slowPackLaneName))
  val fastPick = Resource(fastPickLaneName, 8).add(StationaryDistributionMonitor(fastPickLaneName))
    .add(SojournMonitor(fastPickLaneName))

  val fastPack = Resource(fastPackName, 4).add(StationaryDistributionMonitor(fastPackName))
    .add(SojournMonitor(fastPackName))


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


  //  def slowAndFast(): Unit = {
  //    val buffer = Resource(bufferZoneName, 1000).add(StationaryDistributionMonitor(bufferZoneName)).add
  // (SojournMonitor
  //    (bufferZoneName))
  //
  //    val slowToFastName = "Slow to Fast"
  //    val slowToFast = Resource(slowToFastName, 2).add(StationaryDistributionMonitor(slowToFastName)).add
  // (SojournMonitor(slowToFastName))
  //    val fastToSlowName = "Fast to Slow"
  //    val fastToSlow = Resource(fastToSlowName, 2).add(StationaryDistributionMonitor(fastToSlowName)).add
  // (SojournMonitor(fastToSlowName))
  //    val sojournMonitor = SojournMonitor("Sojourn Network")
  //    val warehouse = Network("Warehouse Network")
  //                    .add(fastPick)
  //                    .add(slowPick)
  //                    .add(buffer)
  //                    .add(slowSort)
  //                    .add(slowPack)
  //                    .add(slowToFast)
  //                    .add(fastToSlow)
  //                    .add(fastPack)
  //      .add(OrdersStream("Orders", Distribution.exp(200), NetworkTopology()
  //                                                         .addShares(source, slowPick -> 0.4, fastPick -> 0.6)
  //                                                         .addShares(slowPick, buffer -> 0.9, slowToFast -> 0.1)
  //                                                         .addShares(fastPick, fastPack -> 0.9, fastToSlow -> 0.1)
  //                                                         .addTransition(slowToFast, fastPick)
  //                                                         .addTransition(fastToSlow, slowPick)
  //                                                         .addShares(buffer, slowPack -> 0.3, slowSort -> 0.7)
  //                                                         .addTransition(slowSort, slowPack)
  //                                                         .addTransition(slowPack, sink)
  //                                                         .addTransition(fastPack, sink)
  //                                                         .addService(fastPick, Distribution.exp(50))
  //                                                         .addService(fastToSlow, Distribution.exp(10))
  //                                                         .addService(slowPick, Distribution.exp(10))
  //                                                         .addService(slowSort, Distribution.exp(8))
  //                                                         .addService(slowPack, Distribution.exp(12))
  //                                                         .addService(slowToFast, Distribution.exp(10))
  //                                                         .addService(fastPack, Distribution.exp(40))
  //                                                         .addService(buffer, Distribution.exp(5))
  //      ))
  //      .add(sojournMonitor)
  //
  ////    val resultTry = Solver.prodForm(warehouse)
  //
  //    val result = resultTry.get
  //    val sojourn = result.results(sojournMonitor).map({
  //      case ContinuousEstimation(_, distribution) => distribution match {
  //        case exp: RichExponential => exp.mean
  //        case lap: LaplaceBasedDistribution => lap.mean
  //      }
  //      case _ => Double.NaN
  //    })
  //    sojourn.foreach(sj => println(s"Network Response Time: $sj"))
  //
  //    val idles = warehouse.resources.flatMap(resource => {
  //      resource.monitors.filter(_.isInstanceOf[StationaryDistributionMonitor]).map({
  //        case monitor: StationaryDistributionMonitor =>
  //          result.results(monitor).map({ case StationaryDistributionEstimation(_, distribution) =>
  //            (monitor.name, distribution(0))
  //          })
  //        case _ => Try(("", 0.0))
  //      })
  //    })
  //    for (idle <- idles;
  //         resourceResult <- idle
  //    ) println(s"The ${resourceResult._1} has been idle: ${resourceResult._2}")
  //
  //    val meanResponses = warehouse.resources.flatMap(resource => {
  //      resource.monitors.filter(_.isInstanceOf[SojournMonitor]).map({
  //        case monitor: SojournMonitor =>
  //          result.results(monitor).map({ case ContinuousEstimation(_, distribution) => distribution match {
  //            case exp: RichExponential => (monitor.name, exp.mean)
  //            case lap: LaplaceBasedDistribution => (monitor.name, lap.mean)
  //          }
  //          })
  //        case _ => Try(("", 0.0))
  //      })
  //    })
  //    for (meanResponse <- meanResponses;
  //         resourceResult <- meanResponse
  //    ) println(s"The ${resourceResult._1} has been responded: ${resourceResult._2}")
  //
  //  }
  //
  //  def onlySlow(): Unit = {
  //    val slowPick = Resource(slowPickLaneName, 20).add(StationaryDistributionMonitor(slowPickLaneName)).add
  // (SojournMonitor(slowPickLaneName))
  //    val buffer = Resource(bufferZoneName, 1000).add(StationaryDistributionMonitor(bufferZoneName)).add
  // (SojournMonitor(bufferZoneName))
  //    val slowSort = Resource(slowSortLaneName, 8).add(StationaryDistributionMonitor(slowSortLaneName)).add
  // (SojournMonitor(slowSortLaneName))
  //    val slowPack = Resource(slowPackLaneName, 8).add(StationaryDistributionMonitor(slowPackLaneName)).add
  // (SojournMonitor(slowPackLaneName))
  //    val sojournMonitor = SojournMonitor("Sojourn Network")
  //    val warehouse = Network("Warehouse Network")
  //      .add(slowPick)
  //      .add(buffer)
  //      .add(slowSort)
  //      .add(slowPack)
  //      .add(OrdersStream("Orders", Distribution.exp(80), NetworkTopology()
  //        .addTransition(source, slowPick)
  //        .addTransition(slowPick, buffer)
  //        .addShares(buffer, slowPack -> 0.3, slowSort -> 0.7)
  //        .addTransition(slowSort, slowPack)
  //        .addTransition(slowPack, sink)
  //        .addService(slowPick, Distribution.exp(10))
  //        .addService(slowSort, Distribution.exp(8))
  //        .addService(slowPack, Distribution.exp(12))
  //        .addService(buffer, Distribution.exp(5))
  //      ))
  //      .add(sojournMonitor)
  //
  //    val resultTry = Solver.prodForm(warehouse)
  //
  //    val result = resultTry.get
  //    val sojourn = result.results(sojournMonitor).map({
  //      case ContinuousEstimation(_, distribution) => distribution match {
  //        case exp: RichExponential => exp.mean
  //        case lap: LaplaceBasedDistribution => lap.mean
  //      }
  //      case _ => Double.NaN
  //    })
  //    sojourn.foreach(sj => println(s"Network Response Time: $sj"))
  //
  //    val idles = warehouse.resources.flatMap(resource => {
  //      resource.monitors.filter(_.isInstanceOf[StationaryDistributionMonitor]).map({
  //        case monitor: StationaryDistributionMonitor =>
  //          result.results(monitor).map({ case StationaryDistributionEstimation(_, distribution) =>
  //            (monitor.name, distribution(0))
  //          })
  //        case _ => Try(("", 0.0))
  //      })
  //    })
  //    for (idle <- idles;
  //         resourceResult <- idle
  //    ) println(s"The ${resourceResult._1} has been idle: ${resourceResult._2}")
  //
  //    val meanResponses = warehouse.resources.flatMap(resource => {
  //      resource.monitors.filter(_.isInstanceOf[SojournMonitor]).map({
  //        case monitor: SojournMonitor =>
  //          result.results(monitor).map({ case ContinuousEstimation(_, distribution) => distribution match {
  //            case exp: RichExponential => (monitor.name, exp.mean)
  //            case lap: LaplaceBasedDistribution => (monitor.name, lap.mean)
  //          }
  //          })
  //        case _ => Try(("", 0.0))
  //      })
  //    })
  //    for (meanResponse <- meanResponses;
  //         resourceResult <- meanResponse
  //    ) println(s"The ${resourceResult._1} has been responded: ${resourceResult._2}")
  //
  //  }
}
