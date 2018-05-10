package solvers.markov

import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, CompRowMatrix, FlexCompRowMatrix, Preconditioner}
import no.uib.cipr.matrix.{Matrix, Vector, sparse}

/**
  * Configuration of a Markov chain solver
  */
class SolverConfig(val solverAlgorithmConfig: SolverAlgorithmConfig,
                   val preconditionerConfig: PreconditionerConfig,
                   val toleranceConfig: ToleranceConfig) {
  protected var preconditioner: Preconditioner = _

  def setPreconditioner(matrix: Matrix): Unit = {
    preconditioner = PreconditionerFactory.getPreconditioner(preconditionerConfig)
    preconditioner.setMatrix(matrix)
  }
  def getSolver(template: Vector): AbstractIterativeSolver = {
    val solver = SolverFactory.getSolverAlgorithm(template, solverAlgorithmConfig)
    solver.setPreconditioner(preconditioner)
    solver
  }
}

sealed trait SolverAlgorithmConfig
case object BiCG extends SolverAlgorithmConfig
case object BiCGStab extends SolverAlgorithmConfig
case object CGS extends SolverAlgorithmConfig
case class GMRES(restart: Int = 30) extends SolverAlgorithmConfig
case object IR extends SolverAlgorithmConfig
case object QMR extends SolverAlgorithmConfig

sealed trait PreconditionerConfig
case class Diagonal(dim: Int) extends PreconditionerConfig
case class AMG(omegaPreF: Double = 1, omegaPreR: Double = 1.85, omegaPostF: Double = 1.85,
               omegaPostR: Double = 1, nu1: Int = 1, nu2: Int = 1, gamma: Int = 1, min: Int = 40,
               omega: Double = 2.0 / 3.0) extends PreconditionerConfig
case class ICC(matrix: Matrix) extends PreconditionerConfig
case class ILU(matrix: Matrix) extends PreconditionerConfig
case class ILUT(matrix: Matrix, tau: Double = 1e-6, p: Int = 25) extends PreconditionerConfig

case class ToleranceConfig(maxIter: Int, absoluteTolerance: Double, divergenceTolerance: Double = 10)

object ToleranceConfig {
  val default: ToleranceConfig = ToleranceConfig(200, 1e-8)
}

object SolverFactory {
  def getSolverAlgorithm(template: Vector, solverType: SolverAlgorithmConfig): AbstractIterativeSolver = {
    solverType match {
      case BiCG => new sparse.BiCG(template)
      case BiCGStab => new sparse.BiCGstab(template)
      case CGS => new sparse.CGS(template)
      case GMRES(restart) => new sparse.GMRES(template, restart)
      case IR => new sparse.IR(template)
      case QMR => new sparse.QMR(template)
    }
  }
}

// TODO hibakezelés rossz param adásakor
object PreconditionerFactory {
  def getPreconditioner(preconditionerType: PreconditionerConfig): Preconditioner = {
    preconditionerType match {
      case Diagonal(dim) => new sparse.DiagonalPreconditioner(dim)
      case AMG(omegaPreF, omegaPreR, omegaPostF, omegaPostR, nu1, nu2, gamma, min, omega) =>
        new sparse.AMG(omegaPreF, omegaPreR, omegaPostF, omegaPostR, nu1, nu2, gamma, min, omega)
      case ICC(matrix) => new sparse.ICC(matrix.asInstanceOf[CompRowMatrix])
      case ILU(matrix) => new sparse.ILU(matrix.asInstanceOf[CompRowMatrix])
      case ILUT(matrix, tau, p) => new sparse.ILUT(matrix.asInstanceOf[FlexCompRowMatrix], tau, p)
    }
  }
}
