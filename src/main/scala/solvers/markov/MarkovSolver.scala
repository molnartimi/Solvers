package solvers.markov

import no.uib.cipr.matrix.{DenseVector, NotConvergedException, Vector, ZeroVector}
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, DefaultIterationMonitor, OneNormNormalizer}

class MarkovSolver(ctmc: CTMC, config: SolverConfig) {

  def solve: Either[SolverError, Vector] = {
    try {
      val solver: AbstractIterativeSolver = createSolver

      val solution = solver.solve(ctmc.getMatrix, b, x)

      normalize(solution)
      negateIfNegative(solution)
      if (!isValidSolution(solution))
        Left(NotAMarkovChainError)

      Right(solution)
    } catch {
      case _: NotConvergedException => Left(NotConvergedError)
      case error: Throwable => {
        error.printStackTrace()
        Left(OtherError)
      }
    }
  }

  protected def createSolver: AbstractIterativeSolver = {
    config.setPreconditioner(ctmc.getMatrix)
    val solver: AbstractIterativeSolver = config.getSolver(x)

    solver.setNormalizer(new OneNormNormalizer())

    solver.setIterationMonitor(new DefaultIterationMonitor(
      config.toleranceConfig.maxIter,
      0,
      config.toleranceConfig.absoluteTolerance,
      config.toleranceConfig.divergenceTolerance))
    solver
  }

  protected def b: Vector = new ZeroVector(ctmc.size)

  protected def x: Vector = new DenseVector(Array.fill[Double](ctmc.size)(1.0 / ctmc.size))

  protected def normalize(vector: Vector): Vector = vector.scale(1.0 / vector.norm(Vector.Norm.One))

  protected def negateIfNegative(vector: Vector): Unit = {
    var i = 0
    var done = false

    while (i < vector.size && !done) {
      if (vector.get(i) < 0) {
        vector.scale(-1)
        done = true
      }
      i += 1
    }
  }

  protected def isValidSolution(vector: Vector): Boolean = {
    var i = 0
    while (i < vector.size && vector.get(i) >= 0)
      i += 1

    i == vector.size
  }
}

