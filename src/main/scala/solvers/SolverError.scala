package solvers

sealed trait SolverError
case object NotAMarkovChain extends SolverError
case class ReducibleMarkovChain(kernelDimension: Int) extends SolverError
