package qn

import breeze.stats.distributions.{ContinuousDistr, Moments}
import qn.monitor.Monitor

sealed trait Trajectory {
  def add(monitor: Monitor):Trajectory
  def monitors:List[Monitor]
}

case class Transition(from:Resource, to:Resource, share:Double)

case class NetworkTopology(name:String = "", services:Map[Resource, ContinuousDistr[Double] with Moments[Double, Double]] = Map(), transitions: Set[Transition] = Set(), monitors:List[Monitor] = List()) extends Trajectory {
  override def add(monitor: Monitor): Trajectory = copy(monitors = monitor :: monitors)

  def add(transition: Transition): NetworkTopology = copy(transitions = transitions + transition)

  def addTransition(from: Resource, to: Resource): NetworkTopology = copy(transitions = transitions + Transition(from, to, 1.0))

  def addShares(from: Resource, pairs: (Resource, Double)*): NetworkTopology = copy(transitions = transitions ++ pairs.map(p => Transition(from, p._1, p._2)))

  def addService(resource: Resource, distribution: ContinuousDistr[Double] with Moments[Double, Double]): NetworkTopology = copy(services = services + (resource -> distribution))
}