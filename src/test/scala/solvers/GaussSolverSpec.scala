package solvers

class GaussSolverSpec extends SolverTest(GaussSolver, "gaussian elimination") {
  it should "give ReducibleMarkovChain answer if Markov chain is reducible" in {
    val Left(solution) = GaussSolver.solveSteadyState(INVALID_REDUCIBLE_4x4_MC)
    solution shouldBe ReducibleMarkovChain(2)
  }
}