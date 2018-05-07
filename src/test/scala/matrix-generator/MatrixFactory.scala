package generated
import no.uib.cipr.matrix
import no.uib.cipr.matrix.{DenseMatrix, DenseVector}
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix

object MatrixFactory {
  type TestData = (LinkedSparseMatrix, matrix.DenseVector)

  var MU = 1.0
  var LAMBDA = 2.0

  def makeBinary(n: Int): TestData = {
    (toSparseMatrix(generateBinary(n, MU, LAMBDA)), toSparseVector(solution(n, MU, LAMBDA)))
  }

  def makeBinary(n: Int, mu: Double, lambda: Double): TestData = {
    (toSparseMatrix(generateBinary(n, mu, lambda)), toSparseVector(solution(n, mu, lambda)))
  }

  private def toSparseMatrix(matrix: Array[Array[Double]]): LinkedSparseMatrix = {
    new LinkedSparseMatrix(new DenseMatrix(matrix))
  }

  private def toSparseVector(value: Array[Double]): DenseVector = {
    new DenseVector(value)
  }

  private def generateBinary(n: Int, mu: Double, lambda: Double): Array[Array[Double]] = {
    val matrix = Array.ofDim[Double](n, n)
    for (i <- 0 until n) {
      val binaryString = toBinaryString(i, n)
      for (j <- 0 until maxBinLength(n)) {
        val inverse = if (binaryString(j) == '0') '1' else '0'
        val state = Integer.parseInt(binaryString.substring(0, Math.max(0,j)) + inverse + binaryString.substring(j+1, maxBinLength(n)), 2)
        if (state < n) {
          matrix(i)(state) = if (inverse == '0') mu else lambda
          matrix(i)(i) -= matrix(i)(state)
        }
      }
    }
    matrix
  }

  private def solution(n: Int, mu: Double, lambda: Double): Array[Double] = {
    var solution = Array.ofDim[Double](n)
    for (i <- 0 until n) {
      val binX = toBinaryString(i, n)
      var prod = 1.0
      for (j <- binX) {
        prod *= ( if (j == '0') mu else lambda )
      }
      solution(i) = prod
    }
    val sum = solution.sum
    solution = solution.map(x => x/sum)
    solution
  }

  private def maxBinLength(dim: Double): Int = {
    (Math.floor(Math.log(dim) / Math.log(2.0)) + 1).toInt
  }

  private def toBinaryString(x: Int, dim: Int): String = {
    var binX = x.toBinaryString
    while (binX.length < maxBinLength(dim)) {
      binX = '0' + binX
    }
    binX
  }
}
