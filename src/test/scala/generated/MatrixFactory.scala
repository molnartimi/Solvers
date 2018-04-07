package generated

import breeze.linalg.{DenseMatrix, DenseVector}
import solvers.SolverError

object MatrixFactory {
  type BreezeResult = Either[(DenseMatrix[Double], DenseVector[Double]), SolverError]

  def makeBinary(n: Int): DenseMatrix[Double] = {
    toDenseMatrix(generateBinary(n))
  }

  def makeSimple(n: Int): DenseMatrix[Double] = {
    toDenseMatrix(generateSimple(n))
  }

  private def toDenseMatrix(matrix: Array[Array[Double]]): DenseMatrix[Double] = {
    val n = matrix.length
    val result = Array.ofDim[Double](n * n)
    for(i <- 0 until n; j <- 0 until n) {
      result(i * n + j) = matrix(j)(i)
    }

    new DenseMatrix(n, n, result)
  }

  private def generateBinary(n: Int): Array[Array[Double]] = {
    val matrix = Array.ofDim[Double](n, n)

    matrix(0) = Array[Double](-(n-1)) ++ Array.fill[Double](n-1)(1)

    for(i <- 1 until n) {
      matrix(i)(0) = 1

      var j = 1
      var num = i-1
      var cnt = 1
      while(num > 0) {
        var mod = num % 2
        matrix(i)(j) = mod
        cnt += mod
        j += 1
        num /= 2
      }
      matrix(i)(i) = -cnt
    }

    matrix
  }

  private def generateSimple(n: Int): Array[Array[Double]] = {
    var matrix = Array.ofDim[Double](n, n)

    matrix(0)(0) = -1
    matrix(0)(1) = 1
    for(i <- 1 until n-1) {
      matrix(i)(i-1) = 1
      matrix(i)(i) = -2
      matrix(i)(i+1) = 1
    }
    matrix(n-1)(n-1) = -1
    matrix(n-1)(n-2) = 1

    matrix
  }
}
