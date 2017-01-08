package qn.sim.network

case class CombineNetworkQuery(queries: List[NetworkQuery]) extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = queries.foreach(_.append(networkStateEvent))
}
