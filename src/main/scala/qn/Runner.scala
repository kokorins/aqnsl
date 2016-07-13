package qn

import breeze.stats.distributions.Exponential
import qn.distribution.Distribution
import qn.monitor.{SojournEstimation, SojournMonitor, StationaryDistributionEstimation, StationaryDistributionMonitor}
import qn.solver.Solver

object Runner {

  import Distribution._
  import Resource._

  def main(args: Array[String]) {
    val bagSorter = Resource("Bag Sorter", 8).add(StationaryDistributionMonitor("Bag Sorter"))
    val kola = Resource("KoLa", 20).add(StationaryDistributionMonitor("KoLa"))
    val hds = Resource("HDS", 1000).add(StationaryDistributionMonitor("HDS"))
    val manualSort = Resource("Manual Sort", 8).add(StationaryDistributionMonitor("MS"))
    val manualPack = Resource("Manual Pack", 8).add(StationaryDistributionMonitor("MP"))
    val kola2bag = Resource("Container to Bag", 2).add(StationaryDistributionMonitor("C2B"))
    val bag2kola = Resource("Bag to Container", 2).add(StationaryDistributionMonitor("B2C"))
    val bagPack = Resource("Bag Packing", 4).add(StationaryDistributionMonitor("BP"))
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
      .add(OrdersStream("Orders", Distribution.exp(100), NetworkTopology()
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
        .addService(manualSort, Distribution.exp(4))
        .addService(manualPack, Distribution.exp(10))
        .addService(kola2bag, Distribution.exp(10))
        .addService(bagPack, Distribution.exp(20))
        .addService(hds, Distribution.exp(5))
      ))
      .add(sojournMonitor)

    val resultTry = Solver.prodForm(warehouse)

    val result = resultTry.get
    val sojourn = result.results(sojournMonitor).map({
      case SojournEstimation(_, distribution) => distribution match {
        case exp: Exponential => exp.mean
      }
      case _ => Double.NaN
    })
    sojourn.foreach(sj => println(s"Network Response Time: $sj"))
    val idles = warehouse.resources.flatMap(resource => {
      resource.monitors.map({
        case monitor: StationaryDistributionMonitor =>
          result.results(monitor).map({ case StationaryDistributionEstimation(_, distribution) =>
            (monitor.name, distribution(0))
          })
      })
    })
    for (idle <- idles;
         resourceResult <- idle
    ) println(s"The ${resourceResult._1} has been idle $resourceResult")
  }
}
