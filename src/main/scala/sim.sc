import qn.Resource.{sink, source}
import qn._
import qn.distribution.Distribution
import qn.sim.network.CombinedNetworkQuery
import qn.sim.network.estimator.{BacklogEstimator, ProcessedEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}

val server: Resource = Resource("Server", 1)

val networkName = "MM1"
val mm1 = Network(networkName)
          .add(server)
          .add(OrdersStream("Orders", Distribution.exp(0.8),
            NetworkTopology()
            .add(Transition(source, server, 1.0))
            .add(Transition(server, sink, 1.0))
            .addService(server, Distribution.exp(1.0))
          ))

val serverBacklog = BacklogEstimator(server)
val networkSojourn = SojournEstimator(networkName)
val networkProcessed = ProcessedEstimator(networkName)
val sim = Simulator(mm1, SimulatorArgs(CombinedNetworkQuery(List(networkSojourn, networkProcessed)), Map(server -> serverBacklog), 100.0))
sim.simulate()

networkSojourn.estimate
networkProcessed.estimate
serverBacklog.estimate