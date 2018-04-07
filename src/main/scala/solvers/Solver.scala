package solvers

import wrapper.{Matrix, Vector}

sealed trait SolverError
case object NotAMarkovChain extends SolverError
case class ReducibleMarkovChain(kernelDimension: Int) extends SolverError

trait Solver {
  def solveSteadyState(q: Matrix): Solver.Solution

  protected def validResultVector(vec: Vector): Boolean = {
    var pos = false
    var neg = false
    for (i <- 0 until vec.size()) {
      pos = pos || vec(i) > 0
      neg = neg || vec(i) < 0
    }
    !(pos && neg)
  }
}

object Solver {
  type Solution = Either[SolverError, Vector]

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
