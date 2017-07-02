package qn.dot

import qn.{NetworkGraph, Resource}

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

object DotTransformer {
  val fontAttr = DotAttr("fontname", "PT Sans Narrow")
  val queueAttr = Seq(DotAttr("style", "filled"))

  type NetworkRepr = Graph[String, WDiEdge]
  def queueName(node: String) = s"${node}_queue"
  def serverName(node: String) = s"${node}_server"
  def clusterName(node: String) = s"cluster_${node.replace("_queue", "").replace("_server", "")}"

  def toDot(network: NetworkGraph): String = {
    val root = DotRootGraph(directed = true, Option(network.name),
      attrList = Seq(DotAttr("rankdir", "LR"), DotAttr("compound", "true")),
      attrStmts = Seq(DotAttrStmt(Elem.graph, Seq(fontAttr, DotAttr("fontsize", 36), DotAttr("labelloc", "t"))),
        DotAttrStmt(Elem.node, Seq(DotAttr("shape", "box"), DotAttr("style", "rounded,filled"), DotAttr("fillcolor",
          "\"#333333\""), DotAttr("fontcolor", "#ffffff"), fontAttr))))
    def edgeTransformer(innerEdge: NetworkRepr#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
      innerEdge.edge match {
        case w: WDiEdge[Resource] => {
          val source = w._1.value
          val target = w._2.value
          val weight = 1.0 * w.weight / Long.MaxValue
          val attrs = Seq(DotAttr("ltail", clusterName(source)), DotAttr("lhead", clusterName(target)))
          val weightAttr = if (weight < 1) {
            Seq(DotAttr("label", weight))
          } else {
            Nil
          }
          Some((root, DotEdgeStmt(source, target, attrs ++ weightAttr)))
        }
      }

    def nodeTransformer(innerNode: NetworkRepr#NodeT): Option[(DotGraph, DotNodeStmt)] = {
      val id: String = clusterName(innerNode.value)
      Some((DotSubGraph(ancestor = root, subgraphId = id, attrList = Seq()), DotNodeStmt(innerNode.value, Nil)))
    }

    val dotNodes = network.graph.nodes
      .map(node => WDiEdge(queueName(node.name), serverName(node.name))(Long.MaxValue))
      .foldLeft(Graph[String, WDiEdge]())(_ + _)
    val dotGraph = network.graph.edges
      .map(edge => WDiEdge(serverName(edge.source.value.name), queueName(edge.target.value.name))(edge.weight))
      .foldLeft(dotNodes)(_ + _)
    dotGraph.toDot(root, edgeTransformer = edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  }
}
