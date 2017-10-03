package qn.examples

import qn.distribution.Distribution
import qn.dot.{DotConfig, DotTransformer}
import qn.{NetworkGraph, Resource}

object NetworkRepresentation {

  def main(args: Array[String]): Unit = networkRepresentation

  private def networkRepresentation = {

    val serverName = "Server"
    val server = Resource(serverName, 1)
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
