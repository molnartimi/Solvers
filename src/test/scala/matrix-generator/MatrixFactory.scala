package generated
import no.uib.cipr.matrix
import no.uib.cipr.matrix.{DenseMatrix, DenseVector}
import no.uib.cipr.matrix.sparse.{CompRowMatrix, LinkedSparseMatrix}

import scala.collection.mutable.ArrayBuffer

object MatrixFactory {
  type TestDataLPM = (LinkedSparseMatrix, matrix.DenseVector)
  type TestDataCRM = (CompRowMatrix, matrix.DenseVector)

  var MU = 1.0
  var LAMBDA = 2.0

  def makeBinary(n: Int): TestDataLPM = {
    (toLinkedSparseMatrix(generateBinary(n, MU, LAMBDA)), toDenseVector(solution(n, MU, LAMBDA)))
  }

  def makeBinary(n: Int, mu: Double, lambda: Double): TestDataLPM = {
    (toLinkedSparseMatrix(generateBinary(n, mu, lambda)), toDenseVector(solution(n, mu, lambda)))
  }

  def makeBinaryCRM(n: Int): TestDataCRM = {
    (toCompRowMatrix(generateBinary(n, MU, LAMBDA)), toDenseVector(solution(n, MU, LAMBDA)))
  }

  def makeBinaryCRM(n: Int, mu: Double, lambda: Double): TestDataCRM = {
    (toCompRowMatrix(generateBinary(n, mu, lambda)), toDenseVector(solution(n, mu, lambda)))
  }

  private def toLinkedSparseMatrix(matrix: Array[Array[Double]]): LinkedSparseMatrix = {
    new LinkedSparseMatrix(new DenseMatrix(matrix))
  }

  private def toCompRowMatrix(matrix: Array[Array[Double]]): CompRowMatrix = {
    val nz = new ArrayBuffer[Array[Int]]()
    for(j <- matrix.indices) {
      val row = new ArrayBuffer[Int]()
      for(i <- matrix.indices; if matrix(i)(j) != 0) {
        row += i
      }
      nz += row.toArray
    }
    val sparseMatrix = new CompRowMatrix(matrix.length, matrix.length, nz.toArray)

    for(j <- matrix.indices; i <- matrix.indices if matrix(i)(j) != 0) {
      sparseMatrix.set(j, i, matrix(i)(j))
    }

    sparseMatrix
  }

  private def toDenseVector(value: Array[Double]): DenseVector = {
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
