package qn.solver

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions._
import com.typesafe.scalalogging.LazyLogging
import qn._
import qn.distribution.Distribution.RichExponential
import qn.distribution.{Distribution, HasLaplaceTransform}
import qn.util.{ImmutableBiMap, SojournUtils}

import scala.collection.mutable
import scala.util.Try
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

trait NetworkProductQuery {
  def calc(network: Network, stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Unit
}

class DefaultQuerySet extends NetworkProductQuery {
  private val nodeSojournQuery: mutable.Map[Resource, NodeSojournProductQuery] = mutable.Map.empty
  private val nodeStationaryQuery: mutable.Map[Resource, NodeStationProductQuery] = mutable.Map.empty
  private val networkSojournQuery = NetworkSojournProductQuery()
  override def calc(network: Network, stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Unit = {
    networkSojournQuery.calc(network, stationaryDistribution)
    stationaryDistribution.keySet.foreach(r => {
      val sojournQuery = NodeSojournProductQuery(r)
      val stationaryQuery = NodeStationProductQuery(r)
      stationaryQuery.calc(network, stationaryDistribution)
      nodeStationaryQuery += r -> stationaryQuery
      nodeSojournQuery += r -> sojournQuery
    })
  }

  def nodesSojourn: Map[Resource, ContinuousDistr[Double] with HasCdf] = nodeSojournQuery
    .flatMap({ case (r, q) => q.distr.map(d => r -> d) })(collection.breakOut)
  def nodesStationary: Map[Resource, DiscreteDistr[Int]] = nodeStationaryQuery
    .flatMap({ case (r, q) => q.distr.map(d => r -> d) })(collection.breakOut)
  def networkSojourn = networkSojournQuery.distr
}

case class CombinedNetworkProductQuery(networkProductQueries: Seq[NetworkProductQuery]) extends NetworkProductQuery {
  override def calc(network: Network,
                    stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Unit = networkProductQueries
    .foreach(_.calc(network, stationaryDistribution))
}

case class NodeStationProductQuery(node: Resource, var distr: Option[DiscreteDistr[Int]] = None)
  extends NetworkProductQuery {
  override def calc(network: Network, stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Unit = {
    distr = Some(stationaryDistribution(node))
  }
}

case class NodeSojournProductQuery(node: Resource,
                                   var distr: Option[Moments[Double, Double] with ContinuousDistr[Double] with
                                     HasLaplaceTransform with HasCdf] = None)
  extends NetworkProductQuery {
  override def calc(network: Network, stationaryDistributions: Map[Resource, DiscreteDistr[Int]]): Unit = {
    val stationaryDistribution = stationaryDistributions(node)
    val service = network.generators.head.trajectory match {
      case NetworkGraph(_, services, _) => services(node)
    }

    distr = Some(service match {
      case serv: RichExponential => stationaryDistribution match {
        case numOrds: Geometric => Distribution.sumRandom(serv.exp, numOrds)
        case _ => throw new IllegalStateException("Stationary distribution is only geometric")
      }
      case _ => throw new IllegalStateException("Service distribution is only exponential")
    })
  }
}

case class NetworkSojournProductQuery(var distr: Option[Moments[Double, Double] with ContinuousDistr[Double] with
  HasLaplaceTransform with HasCdf] = None) extends NetworkProductQuery {
  override def calc(network: Network, stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): Unit = {
    val nodeSojourns = network.resources.map(res => {
      val query = NodeSojournProductQuery(res)
      query.calc(network, stationaryDistribution)
      query.distr
    }).flatten

    val transitions = network.generators.head.trajectory.transitions
    val resourceIdxs: Range = network.resources.indices
    val sourceTransitions = transitions.filter(_.from == Resource.source)
    val incomingProbs = DenseVector(
      (for (idx <- resourceIdxs) yield sourceTransitions.find(_.to == network.resources(idx)).fold(0.0)(_.share))
        .toArray)
    val sinkTransitions = transitions.filter(_.to == Resource.sink)
    val outgoingProbs = DenseVector(
      (for (idx <- resourceIdxs) yield sinkTransitions.find(_.from == network.resources(idx)).fold(0.0)(_.share))
        .toArray)
    val transitionMatrix = DenseMatrix.zeros[Double](network.resources.size, network.resources.size)
    for (i <- resourceIdxs; j <- resourceIdxs)
      transitionMatrix.update(i, j,
        transitions.find(trans => trans.from == network.resources(i) && trans.to == network.resources(j))
          .fold(0.0)(_.share))

    distr = Option(SojournUtils.laplace(incomingProbs, outgoingProbs, transitionMatrix,
      nodeSojourns.map({ case distr: HasLaplaceTransform => distr.laplace })))
  }
}

case class ProductFormSolverArgs(networkProductQuery: NetworkProductQuery)

case class ProductFormSolver(network: Network, args: ProductFormSolverArgs) extends LazyLogging {

  private def calcStationaryDistribution(incomingRates: DenseVector[Double],
                                         transitionMatrix: DenseMatrix[Double],
                                         serviceRates: DenseVector[Double]): Try[Seq[DiscreteDistr[Int]]] = {
    logger.info(s"Node incoming rates: $incomingRates")
    logger.info(s"Transition matrix:\n$transitionMatrix")
    logger.info(s"Service rates: $serviceRates")
    val resourceRates = (DenseMatrix.eye[Double](incomingRates.activeSize) - transitionMatrix).t \ incomingRates

    // Lambda = (I-Q)^-1'.L
    val loads: DenseVector[Double] = resourceRates /:/ serviceRates

    if ((loads >:= 1.0).fold(false)(_ || _)) {
      Try(throw new IllegalStateException("Network is overload: " + loads.toScalaVector().mkString(",")))
    }
    else {
      Try(loads.toArray.toSeq.map(rho => Distribution.geom(1 - rho)))
    }
  }

  private def calcStationaryDistribution(generator: OrdersStream,
                                         services: Map[Resource, ContinuousDistr[Double] with Moments[Double, Double]],
                                         graph: Graph[Resource, WDiEdge]): Try[Map[Resource, DiscreteDistr[Int]]] = {

    val interArrivalDist = generator.distribution
    val seqResources = network.resources.zipWithIndex

    val lambda = 1.0 / interArrivalDist.mean
    val sourceTransitions = graph.get(Resource.source).outgoing
    val incomingRates = DenseVector(seqResources
      .map(resource => sourceTransitions.find(_.to.value == resource._1)
        .fold(0.0)(_.weight.toDouble / Long.MaxValue * lambda))
      .toArray)
    val serviceRates = DenseVector(seqResources.map(_._1).map(resource => resource.numUnits / services(resource).mean).toArray)

    val transitionMatrix = DenseMatrix.zeros[Double](seqResources.size, seqResources.size)

    val resourcesToIdxs = ImmutableBiMap(seqResources.toMap)
    for {
      e <- graph.edges if e.from.value != Resource.source && e.to.value != Resource.sink
    } transitionMatrix
      .update(resourcesToIdxs(e.from.value), resourcesToIdxs(e.to.value), e.weight.toDouble / Long.MaxValue)

    val idxToResources = resourcesToIdxs.inverse
    calcStationaryDistribution(incomingRates, transitionMatrix, serviceRates).map(_.zipWithIndex.map(pair => (idxToResources(pair._2), pair._1)).toMap)
  }

  def solve(): Try[Unit] = {
    if(network.generators.size>1)
      return Try(throw new IllegalStateException("There should be only one trajectory"))
    val generator: OrdersStream = network.generators.head
    generator.trajectory match {
      case NetworkGraph(_, services, graph) =>
        val stationaryDistributionTry = calcStationaryDistribution(generator, services, graph)
        stationaryDistributionTry
          .map(stationaryDistribution => args.networkProductQuery.calc(network, stationaryDistribution))
      case _ => Try(throw new IllegalStateException("Only transition topology is applicable"))
    }
  }

}