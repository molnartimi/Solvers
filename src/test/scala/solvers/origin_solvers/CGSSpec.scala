package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, CGS}
import no.uib.cipr.matrix.sparse.markov.CGSForMarkov

class CGSSpec extends MTJSolversSpec("CGS") {
  override protected def createMySolver(template: matrix.Vector): AbstractIterativeSolver = {
    new CGSForMarkov(template)
  }

  override protected def createOriginSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new CGS(template)
  }
}
