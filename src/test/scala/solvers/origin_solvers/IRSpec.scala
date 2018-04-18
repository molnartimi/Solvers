package solvers.origin_solvers
import no.uib.cipr.matrix
import no.uib.cipr.matrix.sparse.{AbstractIterativeSolver, IR}
import no.uib.cipr.matrix.sparse.markov.IRForMarkov

class IRSpec extends MTJSolversSpec("IR") {
  override protected def createMySolver(template: matrix.Vector): AbstractIterativeSolver = {
    new IRForMarkov(template)
  }

  override protected def createOriginSolver(template: matrix.Vector): AbstractIterativeSolver = {
    new IR(template)
  }
}
