package solvers.origin_solvers
import no.uib.cipr.matrix.Vector
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, BiCG}
import no.uib.cipr.matrix.sparse.markov.BiCGForMarkov

class BiCGSpec extends MTJSolversSpec("BiCG") {
  override protected def createMySolver(template: Vector): AbstractIterativeSolver = {
    new BiCGForMarkov(template)
  }

  override protected def createOriginSolver(template: Vector): AbstractIterativeSolver = {
    new BiCG(template)
  }
}
