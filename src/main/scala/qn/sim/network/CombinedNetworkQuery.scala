package qn.sim.network

case class CombinedNetworkQuery(queries: List[NetworkQuery]) extends NetworkQuery {
  override def append(networkStateEvent: NetworkStateEvent): Unit = queries.foreach(_.append(networkStateEvent))
}
