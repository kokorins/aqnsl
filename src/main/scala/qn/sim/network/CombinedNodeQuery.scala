package qn.sim.network

case class CombinedNodeQuery(queries: List[NodeQuery]) extends NodeQuery {
  override def append(event: NodeStateEvent): Unit = queries.foreach(_.append(event))
}

object CombinedNodeQuery {
  def apply(nodeQueries: NodeQuery*): CombinedNodeQuery = CombinedNodeQuery(nodeQueries.toList)
}
