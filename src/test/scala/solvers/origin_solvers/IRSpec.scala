package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, IR}

class IRSpec extends MTJSolversSpec("IR") {
  override protected def createSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new IR(template)
  }
}
