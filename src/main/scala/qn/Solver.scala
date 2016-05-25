package qn

import scala.util.Try

case class Result(results:Map[Monitor, Try[Distribution]], warnings:Map[Monitor, String])

sealed trait StopCriterion

object Solver {
  def prodForm(network: Network):Try[Result] = ProductFormSolver(network).solve()

  def simulation(network: Network, stopCriterion:StopCriterion):Result = ???
}
