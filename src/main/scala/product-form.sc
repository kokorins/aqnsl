import qn.Resource._
import qn.distribution.Distribution
import qn.monitor.StationaryDistributionMonitor
import qn.solver.{DefaultQuerySet, ProductFormSolver, ProductFormSolverArgs}
import qn.{NetworkTopology, Transition, _}

val server: Resource = Resource("Server", 1)
val stationaryDistNetwork = StationaryDistributionMonitor("Stationary Distribution Network")
val mm1 = Network("MM1")
  .add(server)
  .add(OrdersStream("Orders", Distribution.exp(0.8), NetworkTopology()
    .add(Transition(source, server, 1.0))
    .add(Transition(server, sink, 1.0))))
val default = new DefaultQuerySet()
val resultTry = ProductFormSolver(mm1, ProductFormSolverArgs(default)).solve()
val downtime = default.nodesStationary(server)
downtime.probabilityOf(0)

default.networkSojourn.map(_.mean)