package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.markov.BiCGstabForMarkov
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, BiCGstab}

class BiCGStabSpec extends MTJSolversSpec("BiCGStab") {
  override protected def createMySolver(template: matrix.Vector): AbstractIterativeSolver = {
    new BiCGstabForMarkov(template)
  }

  override protected def createOriginSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new BiCGstab(template)
  }
}
