package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, QMR}
import no.uib.cipr.matrix.sparse.markov.QMRForMarkov

class QMRSpec extends MTJSolversSpec("QMR") {
  override protected def createMySolver(template: matrix.Vector): AbstractIterativeSolver = {
    new QMRForMarkov(template)
  }

  override protected def createOriginSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new QMR(template)
  }
}
