package solvers.breeze_solvers

import solvers.ReducibleMarkovChain

class BreezeDenseSolverGaussSpec extends BreezeDenseSolverSpec(BreezeDenseSolverGauss, "gaussian elimination") {
  it should "give ReducibleMarkovChain answer if Markov chain is reducible" in {
    val Left(solution) = BreezeDenseSolverGauss.solveSteadyState(INVALID_REDUCIBLE_4x4_MC)
    solution shouldBe ReducibleMarkovChain(2)
  }
}