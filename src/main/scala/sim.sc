import breeze.plot._
import qn.Resource.{sink, source}
import qn._
import qn.distribution.Distribution
import qn.monitor.ContinuousEstimation
import qn.sim.network.CombinedNetworkQuery
import qn.sim.network.estimator.{BacklogEstimator, ProcessedEstimator, SojournEstimator}
import qn.sim.{Simulator, SimulatorArgs}

val server: Resource = Resource("Server", 1)

val networkName = "MM1"
val mm1 = Network(networkName)
          .add(server)
          .add(OrdersStream(networkName, Distribution.exp(0.8),
            NetworkTopology()
            .addTransition(source, server)
            .addTransition(server, sink)
            .addService(server, Distribution.exp(1.0))
          ))

val serverBacklog = BacklogEstimator(server)
val networkSojourn = SojournEstimator(networkName)
val networkProcessed = ProcessedEstimator(networkName)
val sim = Simulator(mm1, SimulatorArgs(10000.0, CombinedNetworkQuery(List(networkSojourn, networkProcessed)), Map(server -> serverBacklog)))
sim.simulate()

val sojourn = networkSojourn.estimate.get match {
  case ContinuousEstimation(_, dist) => dist
}
networkProcessed.estimate
serverBacklog.estimate

val f = Figure()
val p = f.subplot(0)
val x = for (i <- 0.0 to 4.0 by 0.1) yield i
p += plot(x, for (t <- x) yield sojourn.unnormalizedPdf(t))

f
