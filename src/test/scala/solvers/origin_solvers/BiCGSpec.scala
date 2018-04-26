package solvers.origin_solvers
import no.uib.cipr.matrix.Vector
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, BiCG}

class BiCGSpec extends MTJSolversSpec("BiCG") {
  override protected def createSolver(template: Vector): AbstractIterativeSolver = {
    new BiCG(template)
  }
}
