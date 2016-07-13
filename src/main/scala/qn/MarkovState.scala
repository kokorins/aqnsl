package qn

import breeze.linalg.Matrix
import breeze.stats.distributions.{ContinuousDistr, Exponential}

case class ContinuousTimeMarkovChain(initial: Matrix[Double],
                                     initialPrev: Matrix[Double],
                                     initialNext: Matrix[Double],
                                     layer: Matrix[Double],
                                     previous: Matrix[Double],
                                     next: Matrix[Double]) {
}

object MarkovProcessConstructor {
  def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)

  def arrival(interArrival: ContinuousDistr[Double]) = interArrival match {
    case Exponential(lambda) => {
      ContinuousTimeMarkovChain(Matrix.zeros[Double](0, 0),
        Matrix.zeros[Double](0, 0),
        Matrix.zeros[Double](0, 0),
        Matrix.create[Double](1, 1, Array(-lambda)),
        Matrix.zeros[Double](1, 1),
        Matrix.create[Double](1, 1, Array(lambda))
      )
    }
  }

  def service(service: ContinuousDistr[Double], numUnits: Int) = service match {
    case Exponential(mu) => {
      val initial = Matrix.zeros[Double](numUnits, numUnits)
      for (i <- 1 until numUnits) {
        initial.update(i, i - 1, mu * i)
        initial.update(i, i, -mu * i)
      }
      val initialNext = Matrix.zeros[Double](1, 1)
      val initialPrev = Matrix.create[Double](1, 1, Array(numUnits * mu))
      val layer = Matrix.create[Double](1, 1, Array(-(numUnits * mu)))
      val previous = Matrix.create[Double](1, 1, Array(numUnits * mu))
      val next = Matrix.zeros[Double](1, 1)
      ContinuousTimeMarkovChain(initial, initialPrev, initialNext, layer, previous, next)
    }
  }

  def system(interArrival: ContinuousDistr[Double], service: ContinuousDistr[Double], numUnits: Int) = {
    interArrival match {
      case Exponential(lambda) => {
        service match {
          case Exponential(mu) => {
            val initial = Matrix.zeros[Double](numUnits, numUnits)
            for (i <- 1 until numUnits) {
              initial.update(i - 1, i, lambda)
              initial.update(i, i - 1, mu * i)
              initial.update(i, i, -(lambda + mu * i))
            }
            initial.update(0, 0, -lambda)
            val initialNext = Matrix.create[Double](1, 1, Array(lambda))
            val initialPrev = Matrix.create[Double](1, 1, Array(numUnits * mu))
            val layer = Matrix.create[Double](1, 1, Array(-(numUnits * mu + lambda)))
            val previous = Matrix.create[Double](1, 1, Array(numUnits * mu))
            val next = Matrix.create[Double](1, 1, Array(lambda))
            ContinuousTimeMarkovChain(initial, initialPrev, initialNext, layer, previous, next)
          }
        }
      }
    }
  }

  def prodSameStates(lhs: ContinuousTimeMarkovChain, rhs: ContinuousTimeMarkovChain): ContinuousTimeMarkovChain = ???
}