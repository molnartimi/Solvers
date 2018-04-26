package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, CGS}

class CGSSpec extends MTJSolversSpec("CGS") {

  override protected def createSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new CGS(template)
  }
}
