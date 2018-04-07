package solvers.matrix_toolkits_java_solvers

import breeze.linalg.sum
import excepctions.InvalidMatrixException
import no.uib.cipr.matrix.Vector.Norm
import no.uib.cipr.matrix.{DenseMatrix, DenseVector}
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix
import solvers.NotAMarkovChain
import solvers.matrix_toolkits_java_solvers.MTJSparseSolver.Solution

class MTJSparseSolverPower (threshold: Double) extends MTJSparseSolver {
  require(threshold > 0, "threshold must be positive")

  override def solveSteadyState(q: LinkedSparseMatrix): Solution = {
    if (q.numRows() != q.numColumns()) {
      return Left(NotAMarkovChain)
    }

    var gamma: Double = 0
    try {
      gamma = chooseGamma(q)
    } catch {
      case _: InvalidMatrixException => return Left(NotAMarkovChain)
      case e: Exception => e.printStackTrace()
    }

    val v: DenseVector = new DenseVector(Array.fill[Double](q.numColumns())(1.0 / q.numRows()))

    var converged = false

    while (!converged) {
      val w: DenseVector = {
        val A = new LinkedSparseMatrix(new DenseMatrix(Array(v.getData)))
        val mult = A.mult(q, new LinkedSparseMatrix(1, q.numColumns()))
        var vec = new DenseVector(q.numColumns())
        for (i <- 0 until A.numColumns()) {
          vec.set(i, mult.get(0, i))
        }
        vec
      }
      converged = w.norm(Norm.Two) <= threshold * gamma
      v.set(v.add(w.scale(1/gamma)))
    }
    return Right(v.scale(1/sum(v)))
  }

  private def chooseGamma(Q: LinkedSparseMatrix): Double = {
    var qMax: Double = 0
    for( i <- 0 until Q.numRows() ) {
      var rowSum: Double = 0
      for ( j <- 0 until Q.numColumns() ) {
        val act = Q.get(i, j)
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

  private def sum(v: DenseVector): Double = {
    var sum = 0.0
    for(i <- 0 until v.size()) {
      sum += v.get(i)
    }
    sum
  }
}

object MTJSparseSolverPower extends MTJSparseSolverPower(1e-10) {
  def withThreshold(threshold: Double): MTJSparseSolverPower = new MTJSparseSolverPower(threshold)
}
