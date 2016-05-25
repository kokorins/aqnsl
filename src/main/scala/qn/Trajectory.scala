package qn

sealed trait Trajectory {
  def add(monitor: Monitor)
}

case class Transition(from:Resource, to:Resource, share:Double)

case class NetworkTopology(name:String = "", services:Map[Resource, Distribution] = Map(), transitions: Set[Transition] = Set(), monitors:List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor) = copy(monitors = monitor :: monitors)
  def add(transition: Transition) = copy(transitions = transitions + transition)
  def addService(resource: Resource, distribution: Distribution) = copy(services = services + (resource->distribution))
}

case class Chain(name:String = "", steps:Seq[Trajectory] = Seq(), monitors: List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor) = Chain(name, steps, monitor::monitors)
  def add(trajectory: Trajectory) = Chain(name, steps :+ trajectory, monitors)
  def seize(resource: Resource, numUnits:Integer, distribution: Distribution, monitors:List[Monitor] = List()) =
    add(Seize(resource, numUnits, Seq(), monitors).add(Timeout(distribution)))
}

case class Seize(resource: Resource, numUnits:Integer, steps: Seq[Trajectory] = Seq(), monitors:List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor) = Seize(resource, numUnits, steps, monitor::monitors)
  def add(trajectory: Trajectory) = Seize(resource, numUnits, steps :+ trajectory, monitors)
}

case class Timeout(distribution: Distribution, monitors: List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor) = Timeout(distribution, monitor :: monitors)
}

case class RandomShares(map:Set[(Double, Trajectory)])

case class Branch[T](shares:T, monitors:List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor) = Branch(shares, monitor::monitors)
}
