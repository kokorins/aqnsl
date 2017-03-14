package qn.util

import breeze.math.Complex
import breeze.numerics.{exp, lgamma, pow}
import galileo.environment.Environment
import galileo.expr.{Expr, Number, Product}

import scala.collection.mutable.ArrayBuffer

object NumericReverseLaplaceTransform {

  def exprCalc(func: Expr, t: Double) = {
    val env = new Environment(Option.empty)
    env.set("t", Number(t))
    func.visit().visit(Option(env)).eval().doubleValue
  }

  def exprComplexCalc(func: Expr, t: Complex) = {
    val env = new Environment(Option.empty)
    env.set("t", galileo.complex.Complex(Number(t.re()), Number(t.im())))
    func.visit().visit(Option(env)).eval()
  }

  def Trapezoidal(f: (Double) => Double, x0: Double, xn: Double, n: Int): Double = {
    val dx: Double = (xn - x0) / n
    (for (i <- 0 until n)
      yield (f(x0 + dx * i) + f(x0 + dx * (i + 1.0))) / 2.0).sum * dx
  }

  def logFact(n: Int): Double = {
    lgamma(n + 1.0)
  }

  def fact(n: Int, prod: Long = 1): Long = {
    if (n < 2)
      prod
    else
      fact(n - 1, prod * n)
  }

  def coef2(numIter: Int, iter: Int) = {
    val elems = for (k <- (iter + 1) / 2 to math.min(iter, numIter / 2)) yield {
      val logNum = numIter / 2.0 * math.log(k) + logFact(2 * k)
      val logDen = logFact(iter - k) + logFact(k - 1) + logFact(k) + logFact(2 * k - iter) + logFact(numIter / 2 - k)
      exp(logNum - logDen)
    }
    val acc = elems.sum
    val term = math.pow(-1, iter + numIter / 2)
    term * acc
  }

  def coef(numIter: Int, iter: Int) = {
    val elems = for (k <- (iter + 1) / 2 to math.min(iter, numIter / 2)) yield {
      val logNum = pow(k, numIter / 2.0) * fact(2 * k)
      val logDen = fact(iter - k) * fact(k - 1) * fact(k) * fact(2 * k - iter) * fact(numIter / 2 - k)
      logNum / logDen
    }
    val acc = elems.sum
    val term = math.pow(-1, iter + numIter / 2)
    term * acc
  }

  // Stehfest 1970
  def stehfestInverse(func: Expr, t: Double, numIter: Int = 6): Double = {
    if (math.abs(t) < Double.MinPositiveValue) {
      0
    } else {
      val lton2 = math.log(2) / t
      val elems = for (i <- 1 to numIter) yield {
        val cn = coef(numIter, i)
        val fn = exprCalc(func, i * lton2)
        cn * fn
      }

      println(elems)
      lton2 * elems.sum
    }
  }

  def stehfest2Inverse(func: Expr, t: Double, numIter: Int = 6): Double = {
    if (math.abs(t) < Double.MinPositiveValue) {
      0
    } else {
      val lton2 = math.log(2) / t
      val elems = for (i <- 1 to numIter) yield {
        val cn = coef2(numIter, i)
        val fn = exprCalc(func, i * lton2)
        cn * fn
      }

      println(elems)
      lton2 * elems.sum
    }
  }

  def talbotInverse(func: Expr, t: Double, m: Int = 64) = {
    val delta = ArrayBuffer.fill(m)(Complex(0.0, 0.0))
    val gamma = ArrayBuffer.fill(m)(Complex(0.0, 0.0))
    delta(0) = Complex(2.0 * m / 5, 0)
    gamma(0) = 0.5 * exp(delta(0))
    for (k <- 1 until m) {
      val a = k * math.Pi / m
      delta(k) = 2.0 * k* m / 5  * Complex(1 / math.tan(a), 1)

      val coef = Complex(1, a * (1 + 1 / pow(math.tan(a), 2)) - 1 / math.tan(a))
      gamma(k) = coef * exp(delta(k))
    }
    val elems = for (k <- 0 until m) yield {
      Product(galileo.complex.Complex(Number(gamma(k).re()), Number(gamma(k).im())), exprComplexCalc(func, delta(k) / t)).simplify match {
        case galileo.complex.Complex(re, _) => re.eval().doubleValue
      }
    }
    0.4 / t * elems.sum
  }


  def stehProb(func: Expr, t: Double, numIter: Int = 6) = {
    if (math.abs(t) < Double.MinPositiveValue)
      0.0
    else {
      val r = Trapezoidal(x => stehfestInverse(func, x), 0, t, numIter)
      r
    }
  }
}
