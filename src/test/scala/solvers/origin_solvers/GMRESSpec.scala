package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, GMRES}

class GMRESSpec extends MTJSolversSpec("GMRES") {
  override protected def createSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new GMRES(template)
  }
}
