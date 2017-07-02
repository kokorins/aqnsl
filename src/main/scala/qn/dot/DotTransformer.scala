package qn.dot

import qn.{NetworkGraph, Resource}

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

trait DotConfig {
  def simple = true
  def showSource = false
  def showSink = false
  def font = "PT Sans Narrow"

  def fontAttr = DotAttr("fontname", font)

  def graphAttrs(title: String, fontSize: Int = 36) = {
    var attrs = Seq(DotAttr("fontsize", fontSize), DotAttr("labelloc", "t"), fontAttr)
    if (title.nonEmpty) {
      attrs :+= DotAttr("label", title)
    }
    DotAttrStmt(Elem.graph, attrs)
  }

  def nodeAttrs = DotAttrStmt(Elem.node,
    Seq(DotAttr("shape", "box"), DotAttr("style", "rounded,filled"), DotAttr("fillcolor",
      "\"#333333\""), DotAttr("fontcolor", "#ffffff"), fontAttr))
}

object DotTransformer {
//  val queueAttr = Seq(DotAttr("style", "filled"))

  type NetworkRepr = Graph[Resource, WDiEdge]
  type QueueNetworkRepr = Graph[String, WDiEdge]
  def queueName(node: String) = s"${node}_queue"
  def serverName(node: String) = s"${node}_server"
  def clusterName(node: String) = s"cluster_${node.replace("_queue", "").replace("_server", "")}"

  def toDot(network: NetworkGraph, conf: DotConfig): String = {
    val root = DotRootGraph(directed = true, Option(network.name),
      attrList = Seq(DotAttr("rankdir", "LR"), DotAttr("compound", "true")),
      attrStmts = Seq(conf.graphAttrs(""), conf.nodeAttrs))
    def edgeTransformer(innerEdge: QueueNetworkRepr#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
      innerEdge.edge match {
        case w: WDiEdge[Resource] => {
          val source = w._1.value
          val target = w._2.value

          if (!conf.showSource) {
            if (w._1 == Resource.source || w._2 == Resource.source) {
              return None
            }
          }
          if (!conf.showSink) {
            if (w._1 == Resource.sink || w._2 == Resource.sink) {
              return None
            }
          }
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

    def simpleEdgeTransformer(innerEdge: NetworkRepr#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
      innerEdge.edge match {
        case w: WDiEdge[Resource] => {
          val source = w._1.value
          val target = w._2.value
          if (!conf.showSource) {
            if (w._1 == Resource.source || w._2 == Resource.source) {
              return None
            }
          }
          if (!conf.showSink) {
            if (w._1 == Resource.sink || w._2 == Resource.sink) {
              return None
            }
          }

          val weight = 1.0 * w.weight / Long.MaxValue
          val attrs = Seq()
          val weightAttr = if (weight < 1) {
            Seq(DotAttr("label", weight))
          } else {
            Nil
          }
          Some((root, DotEdgeStmt(source.name, target.name, attrs ++ weightAttr)))
        }
      }

    def nodeTransformer(innerNode: QueueNetworkRepr#NodeT): Option[(DotGraph, DotNodeStmt)] = {
      val id: String = clusterName(innerNode.value)
      Some((DotSubGraph(ancestor = root, subgraphId = id), DotNodeStmt(innerNode.value, Nil)))
    }

    if (conf.simple) {
      network.graph.toDot(root, edgeTransformer = simpleEdgeTransformer)
    } else {
      val dotNodes = network.graph.nodes
        .map(node => WDiEdge(queueName(node.name), serverName(node.name))(Long.MaxValue))
        .foldLeft(Graph[String, WDiEdge]())(_ + _)
      val dotGraph = network.graph.edges
        .map(edge => WDiEdge(serverName(edge.source.value.name), queueName(edge.target.value.name))(edge.weight))
        .foldLeft(dotNodes)(_ + _)
      dotGraph.toDot(root, edgeTransformer = edgeTransformer, cNodeTransformer = Some(nodeTransformer))
    }
  }
}
