package solvers

class SparseGaussSolverSpec  extends SparseSolverTest(SparseGaussSolver, "sparse gaussian elimination") {
  it should "give ReducibleMarkovChain answer if Markov chain is reducible" in {
    val Left(solution) = SparseGaussSolver.solveSteadyState(INVALID_REDUCIBLE_4x4_MC)
    solution shouldBe ReducibleMarkovChain(2)
  }
}
