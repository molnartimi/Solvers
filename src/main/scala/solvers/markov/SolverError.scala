package solvers.markov

sealed trait SolverError
case object NotAMarkovChainError extends SolverError
case object NotConvergedError extends SolverError
case object OtherError extends SolverError