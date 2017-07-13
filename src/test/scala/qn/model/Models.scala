package qn.model

import qn.distribution.Distribution
import qn.monitor.StationaryDistributionMonitor
import qn.{Network, NetworkTopology, OrdersStream, Resource}

object Models {
  private val stationaryDistribution: StationaryDistributionMonitor = StationaryDistributionMonitor("Pi Monitor")
  private val node1: Resource = Resource("Service unit 1", 1).add(stationaryDistribution)
  private val node2: Resource = Resource("Service unit 2", 1).add(stationaryDistribution)
  private val node3: Resource = Resource("Service unit 3", 1).add(stationaryDistribution)
  val dd1 = Network("DD1")
            .add(node1)
            .add(OrdersStream("Fixed Input, Instant Output", Distribution.deterministic(1.0), NetworkTopology()
                                                                                              .addTransition(Resource.source, node1)
                                                                                              .addTransition(node1, Resource.sink)
                                                                                              .addService(node1, Distribution.deterministic(0.0))))

  val mm1_08 = Network("MM1")
    .add(node1)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, Resource.sink)
      .addService(node1, Distribution.exp(1.0))))

  val mm1mm1 = Network("MM1->MM1")
    .add(node1)
    .add(node2)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, node2)
      .addTransition(node2, Resource.sink)
      .addService(node1, Distribution.exp(1.0))
      .addService(node2, Distribution.exp(1.0))))

  val mm1ormm1 = Network("MM1 or MM1")
    .add(node1)
    .add(node2)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addShares(Resource.source, (node1, 0.5), (node2, 0.5))
      .addTransition(node1, Resource.sink)
      .addTransition(node2, Resource.sink)
      .addService(node1, Distribution.exp(.5))
      .addService(node2, Distribution.exp(.5))))

  val mm1mm1mm1 = Network("MM1->MM1->MM1")
    .add(node1)
    .add(node2)
    .add(node3)
    .add(OrdersStream("Exponential Order Flow", Distribution.exp(0.8), NetworkTopology()
      .addTransition(Resource.source, node1)
      .addTransition(node1, node2)
      .addTransition(node2, node3)
      .addTransition(node3, Resource.sink)
      .addService(node1, Distribution.exp(2))
      .addService(node2, Distribution.exp(1))
      .addService(node3, Distribution.exp(3))))
}
