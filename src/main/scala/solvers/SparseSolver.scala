package solvers

import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix

trait SparseSolver {
  def solveSteadyState(q: LinkedSparseMatrix): SparseSolver.Solution
  protected def validResultVector(vec: DenseVector): Boolean = {
    var pos = false
    var neg = false
    for (x <- vec.getData) {
      pos = pos || x > 0
      neg = neg || x < 0
    }
    return !(pos && neg)
  }
}

object SparseSolver {
  type Solution = Either[SolverError, DenseVector]

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
