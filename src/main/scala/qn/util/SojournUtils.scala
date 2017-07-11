package qn.util

import breeze.linalg
import galileo.expr.Number
import qn.distribution.{LaplaceBasedDistribution, LaplaceReprecentation}

object SojournUtils {
  def laplace(incomeProb: linalg.Vector[Double],
              outgoingProb: linalg.Vector[Double],
              transition: linalg.Matrix[Double],
              nodeLaplaces: Seq[LaplaceReprecentation]): LaplaceBasedDistribution = {
    val gammas = galileo.linalg.DiagMatrix(nodeLaplaces.size, nodeLaplaces.map(_.representation).toList)
    val transExpr = galileo.linalg.DenseMatrix(
      (0 until transition.rows).map(r => (0 until transition.cols).map(c => Number(transition(r, c))).toList).toList)
    val prod = gammas * transExpr
    val mid = galileo.linalg.EyeMatrix(nodeLaplaces.size).toDenseMatrix - prod
    val midInv = mid.inverse
    val half1 = vecToExprVec(incomeProb).transpose.dotProduct(midInv.toDenseMatrix)
    val half2 = gammas * vecToExprVec(outgoingProb)
    val res = half1.dotProduct(half2)
    assert(res.numCols == 1)
    assert(res.numRows == 1)
    LaplaceBasedDistribution(LaplaceReprecentation(res.apply(0, 0)))
  }

  private def vecToExprVec(vec: linalg.Vector[Double]) = {
    galileo.linalg.DenseMatrix(List(vec.toArray.toList.map(Number))).transpose
  }
}
