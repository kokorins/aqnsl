package qn

import breeze.stats.distributions.{ContinuousDistr, Moments}

case class Network(name: String, resources: Seq[Resource] = Seq(), generators: List[OrdersStream] = List()) {
  def add(resource: Resource): Network = Network(name, resources :+ resource, generators)
  def add(generator: OrdersStream): Network = Network(name, resources, generator :: generators)
}

case class OrdersStream(name: String, distribution: ContinuousDistr[Double] with Moments[Double, Double],
                        trajectory: Trajectory)

case class Resource(name: String, numUnits: Int)

object Resource {
  val source = Resource("Source", 0)
  val sink = Resource("Sink", 0)
}

