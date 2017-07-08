package qn.solver

import qn.monitor.{Estimation, Monitor}
import qn.Network

import scala.util.Try

case class Result(results:Map[Monitor, Try[Estimation]], warnings:Map[Monitor, String])

//object Solver {
//  def prodForm(network: Network):Try[Result] = ProductFormSolver(network).solve()
//}
