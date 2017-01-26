package qn.monitor

import breeze.linalg
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{ContinuousDistr, DiscreteDistr, Geometric}
import galileo.expr.Number
import qn.distribution.Distribution.RichExponential
import qn.distribution.{Distribution, HasLaplaceTransform, LaplaceBasedDistribution, LaplaceReprecentation}
import qn.{Network, NetworkTopology, Resource}

sealed trait Monitor {
  def name:String
}

case class NamedMonitor(override val name:String) extends Monitor

sealed trait Estimation {
  def monitor:Monitor
}

case class StationaryDistributionMonitor(name:String) extends Monitor {
  def estimate(resource: Resource, stationaryDistributions: Map[Resource, DiscreteDistr[Int]]) =
    StationaryDistributionEstimation(this, stationaryDistributions(resource))
}

case class StationaryDistributionEstimation(monitor: Monitor, discreteDistr: DiscreteDistr[Int]) extends Estimation

case class SojournMonitor(name:String) extends Monitor {
  def estimate(network: Network, stationaryDistributions: Map[Resource, DiscreteDistr[Int]]): ContinuousEstimation = {
    network.generators.head.trajectory match {
      case NetworkTopology(_, services, transitions, _) => {
        val nodeSojourns = network.resources.map(resource => {
          val stationaryDistribution = stationaryDistributions(resource)
          val service = services(resource)
          service match {
            case serv: RichExponential => stationaryDistribution match {
              case numOrds: Geometric => Distribution.sumRandom(serv.exp, numOrds)
              case _ => throw new IllegalStateException("Stationary distribution is only geometric")
            }
            case _ => throw new IllegalStateException("Service distribution is only exponential")
          }
        })
        val resourceIdxs: Range = network.resources.indices
        val sourceTransitions = transitions.filter(_.from == Resource.source)
        val incomingProbs = DenseVector((for (idx <- resourceIdxs) yield sourceTransitions.find(_.to == network.resources(idx)).fold(0.0)(_.share)).toArray)
        val sinkTransitions = transitions.filter(_.to == Resource.sink)
        val outgoingProbs = DenseVector((for (idx <- resourceIdxs) yield sinkTransitions.find(_.from == network.resources(idx)).fold(0.0)(_.share)).toArray)
        val transitionMatrix = DenseMatrix.zeros[Double](network.resources.size, network.resources.size)
        for (i <- resourceIdxs; j <- resourceIdxs)
          transitionMatrix.update(i, j, transitions.find(trans => trans.from == network.resources(i) && trans.to == network.resources(j)).fold(0.0)(_.share))

        ContinuousEstimation(this, laplaceSojourn(incomingProbs, outgoingProbs, transitionMatrix, nodeSojourns.map({ case distr: HasLaplaceTransform => distr.laplace })))
      }
      case _ => throw new IllegalStateException("Cant work with non network topology")
    }
  }


  def estimate(resource: Resource, service: ContinuousDistr[Double], stationaryDistribution: Map[Resource, DiscreteDistr[Int]]): ContinuousEstimation = service match {
    case RichExponential(expo) =>
      stationaryDistribution(resource) match {
        case geom: Geometric => ContinuousEstimation(this, Distribution.sumRandom(expo, geom))
      }
    case _ => throw new IllegalArgumentException(s"Sojourn distribution is not implemented for such service distribution $service")
    }

  private def laplaceSojourn(incomeProb: linalg.Vector[Double],
                             outgoingProb: linalg.Vector[Double],
                             transition: linalg.Matrix[Double],
                             nodeLaplaces: Seq[LaplaceReprecentation]): LaplaceBasedDistribution = {
    val gammas = galileo.linalg.DiagMatrix(nodeLaplaces.size, nodeLaplaces.map(_.representation).toList)
    val transExpr = galileo.linalg.DenseMatrix((0 until transition.rows).map(r => (0 until transition.cols).map(c => Number(transition(r, c))).toList).toList)
    val prod = gammas * transExpr
    val mid = galileo.linalg.EyeMatrix(nodeLaplaces.size).toDenseMatrix - prod
    val midInv = mid.inverse
    val half1 = vecToExprVec(incomeProb).transpose * midInv
    val half2 = gammas * vecToExprVec(outgoingProb)
    val res = half1 * half2
    assert(res.numCols == 1)
    assert(res.numRows == 1)
    LaplaceBasedDistribution(LaplaceReprecentation(res.apply(0, 0)))
  }

  private def vecToExprVec(vec: linalg.Vector[Double]) = {
    galileo.linalg.DenseMatrix(List(vec.toArray.toList.map(Number))).transpose
  }

}

case class ContinuousEstimation(monitor: Monitor, continuousDistribution: ContinuousDistr[Double]) extends Estimation
case class DiscreteEstimation(monitor: Monitor, discreteDistribution: DiscreteDistr[Int]) extends Estimation