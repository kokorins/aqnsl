package qn.examples

import breeze.plot.{Figure, plot}
import galileo.environment.Environment
import galileo.expr._
import qn.util.NumericReverseLaplaceTransform

object NumericalMethods {
  def main(args: Array[String]): Unit = laplaceTransform

  def laplaceTransform = {
    val ts = 0.0 to 12.0 by 0.1
    val exp = Diff(Number(1), Power(Number(math.E), Product(Number(-1), Variable("t"))))
    val expDens = Product(Number(1), Power(Number(math.E), Product(Number(-1), Variable("t"))))
    val lapExp = Fraction(Number(1), Diff(Number(1), Variable("t")))
    val sinExp = Fraction(Number(1), Sum(Number(1), Power(Variable("t"), Number(2))))

    val expected = for (t <- ts) yield calcAt(exp, t)
    val dens = for (t <- ts) yield calcAt(expDens, t)
    val actual = for (t <- ts) yield NumericReverseLaplaceTransform.stehfestInverse(sinExp, t, 14)
    val actual2 = for (t <- ts) yield NumericReverseLaplaceTransform.stehfest2Inverse(sinExp, t, 14)
    //    val actual3 = for (t <- ts) yield NumericReverseLaplaceTransform.talbotInverse(sinExp, t, 6)
    val figure = Figure("Laplace-Based Functions")
    val laplacePlot = figure.subplot(0)
    laplacePlot += plot(ts, expected, name = "Expected", colorcode = Gray)
    laplacePlot += plot(ts, dens, name = "Density", colorcode = LightGray)
    laplacePlot += plot(ts, actual, name = "Actual", colorcode = Orange)
    laplacePlot += plot(ts, actual2, name = "Actual2", colorcode = LightOrange)
    //    laplacePlot += plot(ts, actual3, name = "Actual3", colorcode = LightOrange)
    println(actual)
    println(actual2)
    //    println(actual3)
    laplacePlot.legend = true
  }

  val Gray = "109, 109, 109"
  val LightGray = "191, 191, 191"
  val Orange = "255, 105, 0"
  val LightOrange = "254, 166, 101"

  private def calcAt(func: Expr, t: Double) = {
    val env = new Environment(Option.empty)
    env.set("t", Number(t))
    func.visit().visit(Option(env)).eval().doubleValue
  }
}
