package qn

import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}
import qn.monitor._

object NetworkRepresentation {

  def main(args: Array[String]): Unit = networkRepresentation

  private def networkRepresentation = {

    val serverName = "Server"
//    val serverSojournMonitor = SojournMonitor(serverName)
    val serverBacklogMonitor = StationaryDistributionMonitor(serverName)
    val server = Resource(serverName, 1)
//      .add(serverSojournMonitor)
      .add(serverBacklogMonitor)
    val networkName = "MM1"
    val networkGraph = NetworkGraph(networkName)
      .addService(server, Distribution.exp(1.0))
      .addTransition(Resource.source, server)
      .addTransition(server, Resource.sink)

    println(DotTransformer.toDot(networkGraph, new DotConfig(){
      override def simple: Boolean = true
    }))
  }
}
