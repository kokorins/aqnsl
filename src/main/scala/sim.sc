import qn.Resource.{sink, source}
import qn._
import qn.distribution.Distribution
import qn.monitor.SojournMonitor
import qn.sim.network.SojournEstimationAppender
import qn.sim.{Simulator, SimulatorArgs}

val server: Resource = Resource("Server", 1)

val mm1 = Network("MM1")
          .add(server)
          .add(OrdersStream("Orders", Distribution.exp(0.8),
            NetworkTopology()
            .add(Transition(source, server, 1.0))
            .add(Transition(server, sink, 1.0))
            .addService(server, Distribution.exp(1.0))
          ))

val networkSojourn = SojournEstimationAppender(SojournMonitor("Network"))
val sim = Simulator(mm1, SimulatorArgs(networkSojourn, 100.0))
sim.simulate()

networkSojourn.estimate