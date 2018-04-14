package solvers.matrix_toolkits_java_solvers

import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix
import solvers.{NotAMarkovChain, ReducibleMarkovChain}
import solvers.matrix_toolkits_java_solvers.MTJSparseSolver.Solution

class MTJSparseSolverGauss(threshold: Double) extends MTJSparseSolver {

  override def solveSteadyState(q: LinkedSparseMatrix): Solution = {
    if (q.numRows() != q.numColumns()) {
      return Left(NotAMarkovChain)
    }

    reducateAndSetMultipliers(q)
    val freeParams =  checkReducibility(q)
    if (freeParams > 1) {
      return Left(ReducibleMarkovChain(freeParams))
    }

    val resultVector = calculateResultVector(q)
    if (!validResultVector(resultVector)) {
      return Left(NotAMarkovChain)
    }

    Right(resultVector.scale(1 / sum(resultVector)))
  }

  private def reducateAndSetMultipliers(q: LinkedSparseMatrix): Unit = {
    for (i <- 0 until q.numRows(); j <- i + 1 until q.numColumns()) {
      q.set(i, j, q.get(i,j) / (-q.get(i, i)))
      for (k <- i + 1 until q.numRows()) {
        q.set(k, j, q.get(k, j) + q.get(i, j) * q.get(k, i))
      }
    }
  }

  private def calculateResultVector(q: LinkedSparseMatrix): DenseVector = {
    val resultVector = new DenseVector(q.numColumns())
    resultVector.set(q.numRows()-1, 1)

    for (j <- q.numColumns()-2 to 0 by -1) {
      for (i <- j + 1 until q.numRows()) {
        resultVector.set(j, resultVector.get(j) + q.get(i, j) * resultVector.get(i))
      }
      resultVector.set(j, resultVector.get(j) / (-q.get(j, j)))
    }

    resultVector
  }

  private def checkReducibility(q: LinkedSparseMatrix): Int = {
    var ctr = 0
    for (row <- 0 until q.numRows()) {
      if (Math.abs(q.get(row, row)) < threshold || q.get(row, row).isNaN) {
        ctr += 1
      }
    }
    ctr
  }

  private def sum(v: DenseVector): Double = {
    var sum = 0.0
    for (i <- 0 until v.size()) {
      sum += v.get(i)
    }
    sum
  }
}

object MTJSparseSolverGauss extends MTJSparseSolverGauss(1e-10) {
  def withThreshold(threshold: Double): MTJSparseSolverGauss = new MTJSparseSolverGauss(threshold)

}