package qn

import qn.distribution.Distribution
import qn.dot.DotTransformer
import qn.monitor._

object NetworkRepresentation {

  def main(args: Array[String]): Unit = networkRepresentation

  private def networkRepresentation = {

    val serverName = "Server"
    val serverSojournMonitor = SojournMonitor(serverName)
    val serverBacklogMonitor = StationaryDistributionMonitor(serverName)
    val server = Resource(serverName, 1)
      .add(serverSojournMonitor)
      .add(serverBacklogMonitor)
    val networkName = "MM1"
//    val networkSojournMonitor = SojournMonitor(networkName)
    val networkGraph = NetworkGraph(networkName)
      .addService(server, Distribution.exp(1.0))
      .addTransition(Resource.source, server)
      .addTransition(server, Resource.sink)
//    val network = Network(networkName, Seq(server), monitors = List(networkSojournMonitor))
//      .add(OrdersStream(networkName, Distribution.exp(0.8), networkGraph))

    println(DotTransformer.toDot(networkGraph))
  }
}
