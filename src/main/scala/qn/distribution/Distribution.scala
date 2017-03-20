package qn.distribution

import breeze.linalg.DenseVector
import breeze.numerics.log
import breeze.stats.distributions.{ContinuousDistr, HasCdf, HasInverseCdf, Moments, _}
import galileo.environment.Environment
import galileo.expr._
import qn.util.NumericReverseLaplaceTransform

case class LaplaceReprecentation(representation: Expr) {
  def moment(k: Int) = {
    derive(k).at(0)
  }

  def derive(k: Int): LaplaceReprecentation = {
    if (k == 0)
      this
    else
      LaplaceReprecentation(Derivative(representation, Variable("t"))).derive(k - 1)
  }

  def at(t: Double) = {
    val env = new Environment(Option.empty)
    env.set("t", Number(t))
    representation.visit().visit(Option(env)).eval().doubleValue
  }
}

trait HasLaplaceTransform {
  def laplace: LaplaceReprecentation
}

case class Erlang(rate: Double, scale: Int) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf with HasInverseCdf with HasLaplaceTransform {
  private val gamma = Gamma(rate, scale)
  override def unnormalizedLogPdf(x: Double): Double = gamma.unnormalizedLogPdf(x)

  override def logNormalizer: Double = gamma.logNormalizer

  override def mean: Double = gamma.mean

  override def variance: Double = gamma.variance

  override def mode: Double = gamma.mode

  override def entropy: Double = gamma.entropy

  override def cdf(x: Double): Double = gamma.cdf(x)

  override def probability(x: Double, y: Double): Double = gamma.probability(x, y)

  override def inverseCdf(p: Double): Double = gamma.inverseCdf(p)

  override def draw(): Double = gamma.draw()

  override def laplace: LaplaceReprecentation = {
    val t = Variable("t")
    val lambda = Number(rate)
    val k = Number(scale)
    LaplaceReprecentation(Power(Fraction(lambda, Diff(lambda, t)), -k))
  }
}

object Distribution {
  def deterministic(dif: Double) = Singular(dif)

  def multi(probs: Double*) = Multinomial(DenseVector(probs.toArray))

  def exp(rate: Double) = RichExponential(Exponential(rate))

  def geom(rho: Double) = Geometric(rho)

  def erlang(rate: Double, num: Integer) = Erlang(rate, num)

  def sum(distribution: Exponential, num: Integer) = erlang(distribution.rate, num)

  def sumRandom(distribution: Exponential, num: Geometric) = exp(num.mean *distribution.rate)

  def thinning(distribution: Exponential, prob: Double) = exp(distribution.rate * prob)

  case class RichExponential(exp: Exponential) extends Moments[Double, Double] with ContinuousDistr[Double] with HasLaplaceTransform with HasCdf {
    override def mean: Double = 1.0 / exp.rate

    override def variance: Double = 1.0 / (exp.rate * exp.rate)

    override def mode: Double = 0

    override def entropy: Double = exp.entropy

    override def unnormalizedLogPdf(x: Double): Double = exp.unnormalizedLogPdf(x)

    override def logNormalizer: Double = exp.logNormalizer

    override def draw(): Double = exp.draw()

    override def laplace: LaplaceReprecentation = {
      val t = Variable("t")
      val lambda = Number(exp.rate)
      LaplaceReprecentation(Fraction(lambda, Diff(lambda, t)))
    }

    override def probability(x: Double, y: Double): Double = exp.probability(x, y)

    override def cdf(x: Double): Double = exp.cdf(x)
  }
}

case class Singular(value: Double)(implicit rand: RandBasis = Rand) extends DiscreteDistr[Double] with Moments[Double, Double] with ContinuousDistr[Double] {
  override def probabilityOf(x: Double): Double = if (value == x) 1.0 else 0.0

  override def mean: Double = value

  override def variance: Double = 0

  override def mode: Double = value

  override def entropy: Double = 0

  override def draw(): Double = value

  override def apply(x: Double): Double = probabilityOf(x)

  override def unnormalizedLogPdf(x: Double): Double = 0

  override def logNormalizer: Double = 1
}

case class EmpiricDistribution(values: Array[Double])(implicit rand: RandBasis = Rand) extends DiscreteDistr[Double] with Moments[Double, Double] {

  override def probabilityOf(x: Double): Double = if (values.size <= 0) 0 else values.count(_ < x) / values.size

  override def draw(): Double = values(rand.randInt(values.size).draw()).toDouble

  override def mean: Double = breeze.stats.mean(values)

  override def variance: Double = breeze.stats.variance(values)

  override def mode: Double = breeze.stats.mode(values).mode

  override def entropy: Double = log(values.size)
}

case class LaplaceBasedDistribution(laplace: LaplaceReprecentation)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf {
  override def unnormalizedLogPdf(x: Double): Double = ???

  override def logNormalizer: Double = ???

  override def mean: Double = laplace.moment(1)

  override def variance: Double = {
    val mean = laplace.derive(1)
    mean.moment(1) - (mean.at(0) * mean.at(0))
  }

  override def mode: Double = ???

  override def entropy: Double = ???

  override def draw(): Double = ???

  override def probability(x: Double, y: Double): Double = NumericReverseLaplaceTransform.stehProb(laplace.representation, y) - NumericReverseLaplaceTransform.stehProb(laplace.representation, x)

  override def cdf(x: Double): Double = NumericReverseLaplaceTransform.stehfestInverse(laplace.representation, x)
}