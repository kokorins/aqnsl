package qn

trait Distribution
trait Statistics

object Distribution {
  def multi(probs: Double*) = ???
  def exp(rate:Double):Distribution = ???
  def geom(rho: Double): Distribution = ???
}

object Statistics {
  def mean(distribution: Distribution):Double = ???
}