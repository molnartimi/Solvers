package solvers.markov

import no.uib.cipr.matrix.{Matrix}

class CTMC(qT: Matrix) {

  def getMatrix: Matrix = qT

  def getProbability(from: Int, to: Int): Double = 1.0 / qT.get(to, from)

  def size: Int = qT.numColumns()
}
