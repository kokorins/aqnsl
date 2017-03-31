package qn

import qn.distribution.Distribution
import qn.monitor._

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

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
    val networkSojournMonitor = SojournMonitor(networkName)
    val networkGraph = NetworkGraph()
      .addService(server, Distribution.exp(1.0))
      .addTransition(Resource.source, server)
      .addTransition(server, Resource.sink)
    val network = Network(networkName, Seq(server), monitors = List(networkSojournMonitor))
      .add(OrdersStream(networkName, Distribution.exp(0.8), networkGraph))

    val root = DotRootGraph(directed = true, Option(networkName), attrList = Seq(DotAttr("rankdir", "LR")), attrStmts = Seq(
      DotAttrStmt(Elem.graph, Seq(DotAttr("fontname", "Helvetica"), DotAttr("fontsize", 36), DotAttr("labelloc", "t"))),
      DotAttrStmt(Elem.node, Seq(DotAttr("shape", "box"), DotAttr("style", "rounded,filled"), DotAttr("fillcolor", "\"#333333\""), DotAttr("fontcolor", "#ffffff"), DotAttr("fontname", "Helvetica")))))

    def edgeTransformer(innerEdge: Graph[Resource, WDiEdge]#EdgeT):
    Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
      case w:WDiEdge[Resource] => {
        val source = w._1.value
        val target = w._2.value
        val weight = 1.0 * w.weight / Long.MaxValue
        Some((root, DotEdgeStmt(source.name, target.name, if(weight<1) List(DotAttr("label", weight)) else Nil)))
      }
    }

    val dot = networkGraph.graph.toDot(root, edgeTransformer)
    println(dot)
  }
}
