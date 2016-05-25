package qn

sealed trait Monitor {
  def name:String
}

case class Sojourn(name:String) extends Monitor

case class StationaryDistribution(name:String) extends Monitor
