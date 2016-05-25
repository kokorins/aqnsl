import breeze.linalg.DenseVector
import breeze.stats.distributions.{Multinomial, RandBasis}
import org.apache.commons.math3.random.MersenneTwister

implicit val rand = new RandBasis(new MersenneTwister(0))
val mrnd = Multinomial(DenseVector[Double](0.4, 0.6))

mrnd.sample(10)

val x = DenseVector(0.4, 0.6)

x(-1)