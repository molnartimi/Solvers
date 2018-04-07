package solvers

class GaussSolverSpec extends SolverSpec(GaussSolver, wrapper.MatrixType.BREEZE, "gaussian elimination") {
  it should "give ReducibleMarkovChain answer if Markov chain is reducible" in {
    val Left(solution) = GaussSolver.solveSteadyState(wrapper.MatrixFactory.create(wrapper.MatrixType.BREEZE, INVALID_REDUCIBLE_4x4_MC))
    solution shouldBe ReducibleMarkovChain(2)
  }
}