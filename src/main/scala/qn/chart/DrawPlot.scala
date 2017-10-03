package qn.chart

import breeze.plot.{Plot, plot => draw}
import org.jfree.chart.axis.NumberTickUnit

object DrawPlot {
  val Gray = "109, 109, 109"
  val LightGray = "191, 191, 191"
  val Orange = "255, 105, 0"
  val LightOrange = "254, 166, 101"

  def ofSojournTime(plot: Plot, xs: Seq[Double], analyticY: Seq[Double], simulatedY: Seq[Double]) = {
    plot += draw(xs, analyticY, name = "Analytical", colorcode = Gray)
    plot += draw(xs, simulatedY, name = "Simulated", colorcode = Orange)

    plot.title = "Sojourn Time"
    plot.xlabel = "Time Units"
    plot.ylabel = "Estimated Probability"
    plot.legend = true
    plot.yaxis.setTickUnit(new NumberTickUnit(0.05))
    plot.yaxis.setAutoRangeIncludesZero(true)
  }

  def ofNumberOfOrders(plot: Plot, xs: Seq[Double], analyticsY: Seq[Double], simulatedY: Seq[Double]) = {
    plot += draw(xs, analyticsY, name = "Analytical", colorcode = Gray)
    plot += draw(xs, simulatedY, name = "Simulated", colorcode = Orange)
    plot.title = "Number of Orders"
    plot.xlabel = "Number of Orders"
    plot.ylabel = "Estimated Probability"
    plot.legend = true
    plot.yaxis.setTickUnit(new NumberTickUnit(0.02))
    plot.yaxis.setAutoRangeIncludesZero(true)
  }
}
