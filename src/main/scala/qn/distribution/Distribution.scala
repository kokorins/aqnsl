package qn.distribution

import breeze.linalg.DenseVector
import breeze.numerics.pow
import breeze.stats.distributions.{ContinuousDistr, HasCdf, HasInverseCdf, Moments, _}

trait HasLaplaceTransform {
  def laplaceAt(x:Double):Double
}

trait HasLaplaceDerivative {
  def laplaceDerivative(power:Int):Double
}

case class Erlang(rate:Double, scale:Int) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf with HasInverseCdf {
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
}

object Distribution {
  def deterministic(dif: Double) = Singular(dif)

  def multi(probs: Double*) = Multinomial(DenseVector(probs.toArray))

  def exp(rate: Double) = RichExponential(Exponential(rate))

  def geom(rho: Double) = Geometric(rho)

  def erlang(rate: Double, num: Integer) = Erlang(rate, num)

  def sum(distribution: Exponential, num: Integer) = erlang(distribution.rate, num)

  def sumRandom(distribution: Exponential, num: Geometric) = Exponential(distribution.rate * num.p)

  def thinning(distribution: Exponential, prob: Double) = Exponential(distribution.rate * prob)

  implicit class RichExponential(exp: Exponential) extends Moments[Double, Double] with ContinuousDistr[Double] with HasLaplaceTransform with HasLaplaceDerivative {
    override def mean: Double = 1.0 / exp.rate

    override def variance: Double = 1.0 / (exp.rate * exp.rate)

    override def mode: Double = 0

    override def entropy: Double = ???

    override def unnormalizedLogPdf(x: Double): Double = exp.unnormalizedLogPdf(x)

    override def logNormalizer: Double = exp.logNormalizer

    override def draw(): Double = exp.draw()

    override def laplaceAt(x: Double): Double = 1.0 / (1 - x / exp.rate)

    override def laplaceDerivative(power: Int): Double = pow(exp.rate, power)
  }
}

case class Singular(value: Double)(implicit rand: RandBasis = Rand) extends DiscreteDistr[Double] with Moments[Double, Double] with ContinuousDistr[Double]{
  override def probabilityOf(x: Double): Double = if (value == x) 1.0 else 0.0

  override def mean: Double = value

  override def variance: Double = 0

  override def mode: Double = value

  override def entropy: Double = 0

  override def draw(): Double = value

  override def unnormalizedLogPdf(x: Double): Double = if(x<value) Double.NegativeInfinity else 0.0

  override def logNormalizer: Double = 1

  override def apply(x: Double): Double = probabilityOf(x)
}

case class EmpiricDistribution(values: Array[Double])(implicit rand: RandBasis = Rand) extends DiscreteDistr[Double] with Moments[Double, Double] {
  override def probabilityOf(x: Double): Double = if (values.size <= 0) 0 else values.count(_ < x) / values.size

  override def draw(): Double = values(rand.randInt(values.size).draw())

  override def mean: Double = breeze.stats.mean(values)

  override def variance: Double = breeze.stats.variance(values)

  override def mode: Double = breeze.stats.mode(values).mode

  override def entropy: Double = ???
}
