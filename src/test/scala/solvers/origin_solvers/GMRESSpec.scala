package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, GMRES}
import no.uib.cipr.matrix.sparse.markov.GMRESForMarkov

class GMRESSpec extends MTJSolversSpec("GMRES") {
  override protected def createMySolver(template: matrix.Vector): AbstractIterativeSolver = {
    new GMRESForMarkov(template)
  }

  override protected def createOriginSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new GMRES(template)
  }
}
