package qn

import breeze.linalg.{DenseMatrix, DenseVector, inv}

import scala.util.Try

case class ProductFormSolver(network: Network) {

  private def calcStationaryDistribution(incomingRates: DenseVector[Double], transitionMatrix: DenseMatrix[Double], serviceRates: DenseVector[Double]):Seq[Distribution] = {
    val resourceRates = inv(DenseMatrix.eye[Double](incomingRates.activeSize) - transitionMatrix.t) * incomingRates
    val loads:DenseVector[Double] = resourceRates :/ serviceRates
    loads.toArray.toSeq.map(rho => Distribution.geom(rho))
  }

  private def calcMonitors(stationaryDistribution: Map[Resource, Distribution]) = ???

  def solve():Try[Result] = {
    if(network.generators.size>1)
      return Try(throw new IllegalStateException("There should be only one trajectory"))
    val generator: OrdersStream = network.generators.head
    generator.trajectory match {
      case NetworkTopology(_, services, transitions, _) => {
        val interArrivalDist = generator.distribution
        val idxToResource = network.resources.zipWithIndex
        val lambda = Statistics.mean(interArrivalDist)
        val sourceTransitions = transitions.filter(_.from == Resource.source)
        val incomingRates = DenseVector((for(idx<-idxToResource.indices) yield sourceTransitions.find(_.to == idxToResource(idx)).fold(0.0)(_.share * lambda)).toArray)
        val serviceRates = DenseVector(idxToResource.indices.map(idx=>Statistics.mean(services(idxToResource(idx)._1))).toArray)
        val transitionMatrix = DenseMatrix(for(
          i<-idxToResource.indices;
          j<-idxToResource.indices
        ) yield transitions.find(trans=> trans.from == idxToResource(i)._1 && trans.to == idxToResource(j)._1).fold(0.0)(_.share))
        val stationaryDistribution = calcStationaryDistribution(incomingRates, transitionMatrix, serviceRates).zipWithIndex.map(pair=>idxToResource(pair._2)._1->pair._1).toMap
        calcMonitors(stationaryDistribution)
      }
      case _ => Try(throw new IllegalStateException("Only transition topology is applicable"))
    }
  }
}