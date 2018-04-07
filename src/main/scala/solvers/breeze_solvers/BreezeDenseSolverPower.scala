package solvers.breeze_solvers

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import excepctions.InvalidMatrixException
import solvers.NotAMarkovChain
import solvers.breeze_solvers.BreezeDenseSolver.Solution

class BreezeDenseSolverPower(threshold: Double) extends BreezeDenseSolver {
  require(threshold > 0, "threshold must be positive")

  override def solveSteadyState(q: DenseMatrix[Double]): Solution = {
    if (q.rows != q.cols) {
      return Left(NotAMarkovChain)
    }

    var gamma: Double = 0
    try {
      gamma = chooseGamma(q)
    } catch {
      case _: InvalidMatrixException => return Left(NotAMarkovChain)
      case e: Exception => e.printStackTrace()
    }

    val v: DenseVector[Double] = DenseVector.fill(q.rows){1.0 / q.rows}

    var converged = false

    while (!converged) {
      val w: DenseVector[Double] = (v.t * q).t
      converged = norm(w) <= threshold * gamma
      v += w / gamma
    }
    return Right(v / sum(v))
  }

  private def chooseGamma(Q: DenseMatrix[Double]): Double = {
    var qMax: Double = 0
    for( i <- 0 until Q.rows ) {
      var rowSum: Double = 0
      for ( j <- 0 until Q.cols ) {
        val act = Q(i, j)
        if ( i != j && act < 0 ) throw new InvalidMatrixException()
        if ( act > qMax ) {
          qMax = act
        }
        rowSum += act
      }
      if ( rowSum > threshold ) throw new InvalidMatrixException()
    }
    return qMax + 2
  }
}

object BreezeDenseSolverPower extends BreezeDenseSolverPower(1e-10) {
  def withThreshold(threshold: Double): BreezeDenseSolverPower = new BreezeDenseSolverPower(threshold)
}