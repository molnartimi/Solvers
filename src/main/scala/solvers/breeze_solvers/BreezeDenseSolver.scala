package solvers.breeze_solvers

import breeze.linalg.{DenseMatrix, DenseVector}
import solvers.{NotAMarkovChain, ReducibleMarkovChain, SolverError}

trait BreezeDenseSolver {
  def solveSteadyState(q: DenseMatrix[Double]): BreezeDenseSolver.Solution

  protected def validResultVector(vec: DenseVector[Double]): Boolean = {
    var pos = false
    var neg = false
    for (x <- vec) {
      pos = pos || x > 0
      neg = neg || x < 0
    }
    return !(pos && neg)
  }
}

object BreezeDenseSolver {
  type Solution = Either[SolverError, DenseVector[Double]]

  def solutionToMessage(solution: Solution): String = solution match {
    case Right(p) => s"Got a solution: $p"

    /* Not a Markov chain IF
     not square OR
     sum of row != 0 OR
     negative element outside the diagonal OR
     solution contains negative and positive elements */
    case Left(NotAMarkovChain) => "Not a Markov chain"

    /* Reducible Markov chain with kernel dimension d IF
      After gauss elimination there are d > 1 free parameters (full zero row)
     */
    case Left(ReducibleMarkovChain(d)) => s"Solution is not unique, there are $d solutions"
  }
}
