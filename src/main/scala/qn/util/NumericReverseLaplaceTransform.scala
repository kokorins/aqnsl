package qn.util

import breeze.math.Complex
import breeze.numerics.{exp, floor, lgamma, pow}
import galileo.expr.Expr

object NumericReverseLaplaceTransform {
  def step(numIter: Int, iter: Int) = {
    def fact(x: Double) = exp(lgamma(x))

    var acc = 0.0
    for (k <- math.round(floor((iter + 1) / 2.0)) to math.round(math.min(iter, floor(numIter / 2.0)))) {
      val num: Double = pow(k.toDouble, numIter / 2.0) * fact(2.0 * k)
      val den = fact(iter - k) * fact(k - 1) * fact(k) * fact(2 * k - iter) * fact(numIter / 2.0 - k)
      acc += (num / den)
    }
    val expo = iter + numIter / 2.0
    val term = Complex(-1, 0).pow(expo)
    val res = term * acc
    res.re()
  }

  def inverse(func: Expr, t: Double, numIter: Int = 6): Double = {
    var acc = 0.0
    val lton2 = math.log(2) / t
    for (i <- 0 to numIter) {
      val a = step(numIter, i)
      val at = i * lton2
      val b = func.eval().doubleValue
      acc += (a * b)
    }

    lton2 * acc
  }
}
