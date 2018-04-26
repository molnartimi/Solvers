package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, QMR}

class QMRSpec extends MTJSolversSpec("QMR") {
  override protected def createSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new QMR(template)
  }
}
