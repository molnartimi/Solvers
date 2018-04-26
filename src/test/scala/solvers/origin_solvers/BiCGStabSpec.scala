package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, BiCGstab}

class BiCGStabSpec extends MTJSolversSpec("BiCGStab") {
  override protected def createSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new BiCGstab(template)
  }
}
