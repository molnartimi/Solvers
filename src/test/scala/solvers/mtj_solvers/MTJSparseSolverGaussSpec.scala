package solvers.mtj_solvers

import solvers.ReducibleMarkovChain
import solvers.matrix_toolkits_java_solvers.MTJSparseSolverGauss

class MTJSparseSolverGaussSpec  extends MTJSparseSolverSpec(MTJSparseSolverGauss, "sparse gaussian elimination") {
  it should "give ReducibleMarkovChain answer if Markov chain is reducible" in {
    val Left(solution) = MTJSparseSolverGauss.solveSteadyState(INVALID_REDUCIBLE_4x4_MC)
    solution shouldBe ReducibleMarkovChain(2)
  }
}
