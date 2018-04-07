package solvers.breeze_solvers

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import solvers.{NotAMarkovChain, ReducibleMarkovChain}
import solvers.breeze_solvers.BreezeDenseSolver.Solution

class BreezeDenseSolverGauss(threshold: Double) extends BreezeDenseSolver {

  override def solveSteadyState(q: DenseMatrix[Double]): Solution = {
    if (q.rows != q.cols) {
      return Left(NotAMarkovChain)
    }

    val gauss = reductionAndSetMultipliers(q)
    val freeParams =  checkReducibility(gauss)
    if (freeParams > 1) {
      return Left(ReducibleMarkovChain(freeParams))
    }

    val resultVector = calculateResultVector(gauss)
    if (!validResultVector(resultVector)) {
      return Left(NotAMarkovChain)
    }

    return Right(resultVector / sum(resultVector))
  }

  private def reductionAndSetMultipliers(q: DenseMatrix[Double]): DenseMatrix[Double] = {
    val matrix = q.copy
    for (i <- 0 until matrix.rows; j <- i + 1 until matrix.cols) {
      matrix(i, j) /= - matrix(i, i)
      for (k <- i + 1 until matrix.rows) {
        matrix(k, j) += matrix(i, j) * matrix(k, i)
      }
    }
    return matrix
  }

  private def calculateResultVector(q: DenseMatrix[Double]): DenseVector[Double] = {
    val resultVector = DenseVector.zeros[Double](q.cols)
    resultVector(-1) = 1

    for (j <- q.cols-2 to 0 by -1) {
      for (i <- j + 1 until q.rows) {
        resultVector(j) += q(i, j) * resultVector(i)
      }
      resultVector(j) /= - q(j, j)
    }

    return resultVector
  }

  private def checkReducibility(q: DenseMatrix[Double]): Int = {
    var ctr = 0
    for (row <- 0 until q.rows) {
        if (Math.abs(q(row, row)) < threshold || q(row, row).isNaN) {
          ctr += 1
        }
      }
    return ctr
  }
}

object BreezeDenseSolverGauss extends BreezeDenseSolverGauss(1e-10) {
  def withThreshold(threshold: Double): BreezeDenseSolverGauss = new BreezeDenseSolverGauss(threshold)

}