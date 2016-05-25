package qn

case class Network(name: String,
                   resources: List[Resource] = List(),
                   generators: List[OrdersStream] = List(),
                   monitors: List[Monitor] = List()) {
  def add(resource: Resource): Network = Network(name, resource :: resources, generators, monitors)
  def add(generator: OrdersStream): Network = Network(name, resources, generator :: generators, monitors)
  def add(monitor: Monitor) = Network(name, resources, generators, monitor :: monitors)
}


case class OrdersStream(name: String, distribution: Distribution, trajectory: Trajectory)

case class Resource(name: String, numUnits: Int, monitors: List[Monitor] = List()) {
  def add(monitor: Monitor) = Resource(name, numUnits, monitor::monitors)
}

object Resource {
  val source = Resource("Source", 0)
  val sink = Resource("Sink", 0)
}

