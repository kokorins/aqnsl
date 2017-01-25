package qn

import breeze.stats.distributions.Exponential
import qn.distribution.{Distribution, LaplaceBasedDistribution}
import qn.monitor.{ContinuousEstimation, SojournMonitor, StationaryDistributionEstimation, StationaryDistributionMonitor}
import qn.solver.Solver

import scala.util.Try

object Runner {

  import Distribution._
  import Resource._

  def KoLaBagSorter(): Unit = {
    val bagSorter = Resource("Bag Sorter", 8).add(StationaryDistributionMonitor("Bag Sorter")).add(SojournMonitor("Bag Sorter"))
    val kola = Resource("KoLa", 20).add(StationaryDistributionMonitor("KoLa")).add(SojournMonitor("KoLa"))
    val hds = Resource("HDS", 1000).add(StationaryDistributionMonitor("HDS")).add(SojournMonitor("HDS"))
    val manualSort = Resource("Manual Sort", 8).add(StationaryDistributionMonitor("MS")).add(SojournMonitor("MS"))
    val manualPack = Resource("Manual Pack", 8).add(StationaryDistributionMonitor("MP")).add(SojournMonitor("MP"))
    val kola2bag = Resource("Container to Bag", 2).add(StationaryDistributionMonitor("C2B")).add(SojournMonitor("C2B"))
    val bag2kola = Resource("Bag to Container", 2).add(StationaryDistributionMonitor("B2C")).add(SojournMonitor("B2C"))
    val bagPack = Resource("Bag Packing", 4).add(StationaryDistributionMonitor("BP")).add(SojournMonitor("BP"))
    val sojournMonitor = SojournMonitor("Sojourn Network")
    val warehouse = Network("Warehouse Network")
      .add(bagSorter)
      .add(kola)
      .add(hds)
      .add(manualSort)
      .add(manualPack)
      .add(kola2bag)
      .add(bag2kola)
      .add(bagPack)
      .add(OrdersStream("Orders", Distribution.exp(200), NetworkTopology()
        .addShares(source, kola -> 0.4, bagSorter -> 0.6)
        .addShares(kola, hds -> 0.9, kola2bag -> 0.1)
        .addShares(bagSorter, bagPack -> 0.9, bag2kola -> 0.1)
        .addTransition(kola2bag, bagSorter)
        .addTransition(bag2kola, kola)
        .addShares(hds, manualPack -> 0.3, manualSort -> 0.7)
        .addTransition(manualSort, manualPack)
        .addTransition(manualPack, sink)
        .addTransition(bagPack, sink)
        .addService(bagSorter, Distribution.exp(50))
        .addService(bag2kola, Distribution.exp(10))
        .addService(kola, Distribution.exp(10))
        .addService(manualSort, Distribution.exp(8))
        .addService(manualPack, Distribution.exp(12))
        .addService(kola2bag, Distribution.exp(10))
        .addService(bagPack, Distribution.exp(40))
        .addService(hds, Distribution.exp(5))
      ))
      .add(sojournMonitor)

    val resultTry = Solver.prodForm(warehouse)

    val result = resultTry.get
    val sojourn = result.results(sojournMonitor).map({
      case ContinuousEstimation(_, distribution) => distribution match {
        case exp: RichExponential => exp.mean
        case lap: LaplaceBasedDistribution => lap.mean
      }
      case _ => Double.NaN
    })
    sojourn.foreach(sj => println(s"Network Response Time: $sj"))

    val idles = warehouse.resources.flatMap(resource => {
      resource.monitors.filter(_.isInstanceOf[StationaryDistributionMonitor]).map({
        case monitor: StationaryDistributionMonitor =>
          result.results(monitor).map({ case StationaryDistributionEstimation(_, distribution) =>
            (monitor.name, distribution(0))
          })
        case _ => Try(("", 0.0))
      })
    })
    for (idle <- idles;
         resourceResult <- idle
    ) println(s"The ${resourceResult._1} has been idle: ${resourceResult._2}")

    val meanResponses = warehouse.resources.flatMap(resource => {
      resource.monitors.filter(_.isInstanceOf[SojournMonitor]).map({
        case monitor: SojournMonitor =>
          result.results(monitor).map({ case ContinuousEstimation(_, distribution) => distribution match {
            case exp: RichExponential => (monitor.name, exp.mean)
            case lap: LaplaceBasedDistribution => (monitor.name, lap.mean)
          }
          })
        case _ => Try(("", 0.0))
      })
    })
    for (meanResponse <- meanResponses;
         resourceResult <- meanResponse
    ) println(s"The ${resourceResult._1} has been responded: ${resourceResult._2}")

  }

  def onlyKoLa(): Unit = {
    val kola = Resource("KoLa", 20).add(StationaryDistributionMonitor("KoLa")).add(SojournMonitor("KoLa"))
    val hds = Resource("HDS", 1000).add(StationaryDistributionMonitor("HDS")).add(SojournMonitor("HDS"))
    val manualSort = Resource("Manual Sort", 8).add(StationaryDistributionMonitor("MS")).add(SojournMonitor("MS"))
    val manualPack = Resource("Manual Pack", 8).add(StationaryDistributionMonitor("MP")).add(SojournMonitor("MP"))
    val sojournMonitor = SojournMonitor("Sojourn Network")
    val warehouse = Network("Warehouse Network")
      .add(kola)
      .add(hds)
      .add(manualSort)
      .add(manualPack)
      .add(OrdersStream("Orders", Distribution.exp(80), NetworkTopology()
        .addTransition(source, kola)
        .addTransition(kola, hds)
        .addShares(hds, manualPack -> 0.3, manualSort -> 0.7)
        .addTransition(manualSort, manualPack)
        .addTransition(manualPack, sink)
        .addService(kola, Distribution.exp(10))
        .addService(manualSort, Distribution.exp(8))
        .addService(manualPack, Distribution.exp(12))
        .addService(hds, Distribution.exp(5))
      ))
      .add(sojournMonitor)

    val resultTry = Solver.prodForm(warehouse)

    val result = resultTry.get
    val sojourn = result.results(sojournMonitor).map({
      case ContinuousEstimation(_, distribution) => distribution match {
        case exp: RichExponential => exp.mean
        case lap: LaplaceBasedDistribution => lap.mean
      }
      case _ => Double.NaN
    })
    sojourn.foreach(sj => println(s"Network Response Time: $sj"))

    val idles = warehouse.resources.flatMap(resource => {
      resource.monitors.filter(_.isInstanceOf[StationaryDistributionMonitor]).map({
        case monitor: StationaryDistributionMonitor =>
          result.results(monitor).map({ case StationaryDistributionEstimation(_, distribution) =>
            (monitor.name, distribution(0))
          })
        case _ => Try(("", 0.0))
      })
    })
    for (idle <- idles;
         resourceResult <- idle
    ) println(s"The ${resourceResult._1} has been idle: ${resourceResult._2}")

    val meanResponses = warehouse.resources.flatMap(resource => {
      resource.monitors.filter(_.isInstanceOf[SojournMonitor]).map({
        case monitor: SojournMonitor =>
          result.results(monitor).map({ case ContinuousEstimation(_, distribution) => distribution match {
            case exp: RichExponential => (monitor.name, exp.mean)
            case lap: LaplaceBasedDistribution => (monitor.name, lap.mean)
          }
          })
        case _ => Try(("", 0.0))
      })
    })
    for (meanResponse <- meanResponses;
         resourceResult <- meanResponse
    ) println(s"The ${resourceResult._1} has been responded: ${resourceResult._2}")

  }

  def main(args: Array[String]): Unit = {
//    KoLaBagSorter()
    onlyKoLa()
  }
}
