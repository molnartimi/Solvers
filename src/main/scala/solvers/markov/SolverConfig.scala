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
    preconditioner = PreconditionerFactory.getPreconditioner(preconditionerConfig, matrix)
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
case object Diagonal extends PreconditionerConfig
case object AMG extends PreconditionerConfig // TODO sok parametere van, most defaultot hasznaljuk
case object ICC extends PreconditionerConfig // TODO kipróbálni mi ez
case object ILU extends PreconditionerConfig
case object ILUT extends PreconditionerConfig // TODO ennek is van több paramétere

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
  def getPreconditioner(preconditionerType: PreconditionerConfig, matrix: Matrix): Preconditioner = {
    preconditionerType match {
      case Diagonal => new sparse.DiagonalPreconditioner(matrix.numRows())
      case AMG => new sparse.AMG()
      case ICC => new sparse.ICC(matrix.asInstanceOf[CompRowMatrix])
      case ILU => new sparse.ILU(matrix.asInstanceOf[CompRowMatrix])
      case ILUT => new sparse.ILUT(matrix.asInstanceOf[FlexCompRowMatrix])
    }
  }
}
