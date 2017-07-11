package qn

import breeze.stats.distributions.{ContinuousDistr, Moments}
import qn.monitor.Monitor

case class Network(name: String,
                   resources: Seq[Resource] = Seq(),
                   generators: List[OrdersStream] = List(),
                   monitors: List[Monitor] = List()) {
  def add(resource: Resource): Network = Network(name, resources :+ resource, generators, monitors)
  def add(generator: OrdersStream): Network = Network(name, resources, generator :: generators, monitors)
  @Deprecated
  def add(monitor: Monitor) = Network(name, resources, generators, monitor :: monitors)
}

case class OrdersStream(name: String, distribution: ContinuousDistr[Double] with Moments[Double, Double], trajectory: Trajectory)

case class Resource(name: String, numUnits: Int, monitors: List[Monitor] = List()) {
  def add(monitor: Monitor) = Resource(name, numUnits, monitor :: monitors)
}

object Resource {
  val source = Resource("Source", 0)
  val sink = Resource("Sink", 0)
}

