package solvers.markov

import no.uib.cipr.matrix.{Matrix}
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix

class CTMC(q: Matrix) {
  // CompressedRowCompMatrix
  protected var Q: LinkedSparseMatrix = new LinkedSparseMatrix(q.transpose)

  def getMatrix: Matrix = q

  def getProbability(from: Int, to: Int): Double = 1.0 / Q.get(to, from)

  def size: Int = Q.numColumns()
}

// Log-ba időt is
