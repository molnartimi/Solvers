package generated
import no.uib.cipr.matrix
import no.uib.cipr.matrix.{DenseMatrix, DenseVector, Matrix}
import no.uib.cipr.matrix.sparse.{CompRowMatrix, LinkedSparseMatrix}

import scala.collection.mutable.ArrayBuffer

/**
  * Generate big matrices for testing.
  * Gives the q transposed.
  */
object MatrixFactory {
  type TestDataLSM = (LinkedSparseMatrix, matrix.DenseVector)
  type TestDataCRM = (CompRowMatrix, matrix.DenseVector)

  var MU = 100.0
  var LAMBDA = 0.01

  def makeBinaryLSM(n: Int): TestDataLSM = {
    makeBinaryLSM(n, MU, LAMBDA)
  }

  def makeBinaryLSM(n: Int, mu: Double, lambda: Double): TestDataLSM = {
    val matrix = new LinkedSparseMatrix(n, n)

    for (j <- 0 until n) {
      val binaryString = toBinaryString(j, n)
      var tempRowSum = 0.0
      for (i <- 0 until maxBinLength(n)) {
        val inverse = if (binaryString(i) == '0') '1' else '0'
        val state = Integer.parseInt(binaryString.substring(0, Math.max(0,i)) + inverse + binaryString.substring(i+1, maxBinLength(n)), 2)
        if (state < n) {
          val value = if (inverse == '0') mu else lambda
          matrix.set(state, j, value)
          tempRowSum -= value
        }
      }
      matrix.set(j, j, tempRowSum)
    }

    (matrix, toDenseVector(solution(n, mu, lambda)))
  }

  def makeBinaryCRM(n: Int): TestDataCRM = {
    makeBinaryCRM(n, MU, LAMBDA)
  }

  def makeBinaryCRM(n: Int, mu: Double, lambda: Double): TestDataCRM = {
    val nz = new ArrayBuffer[ArrayBuffer[Int]]()
    val values = new ArrayBuffer[ArrayBuffer[Double]]()
    for(i <- 0 until n) {
      nz += new ArrayBuffer[Int]()
      values += new ArrayBuffer[Double]()
    }

    for (j <- 0 until n) {
      val binaryString = toBinaryString(j, n)
      var tempRowSum = 0.0
      for (i <- 0 until maxBinLength(n)) {
        val inverse = if (binaryString(i) == '0') '1' else '0'
        val state = Integer.parseInt(binaryString.substring(0, Math.max(0,i)) + inverse + binaryString.substring(i+1, maxBinLength(n)), 2)
        if (state < n) {
          val value = if (inverse == '0') mu else lambda
          nz(state) += j
          values(state) += value
          tempRowSum -= value
        }
      }
      // TODO vajon baj-e, ha nem sorrendben vannak az indexek?
      nz(j) += j
      values(j) += tempRowSum
    }

    val nzArray = Array.ofDim[Array[Int]](n)
    for(i <- 0 until n) {
      nzArray(i) = nz(i).toArray
    }

    val matrix = new CompRowMatrix(n, n, nzArray)

    for(i <- 0 until n) {
      var j = 0
      for(idx <- nzArray(i)) {
        matrix.set(i, idx, values(i)(j))
        j += 1
      }
    }

    (matrix, toDenseVector(solution(n, mu, lambda)))
  }

  private def toCompRowMatrix(matrix: Array[Array[Double]]): CompRowMatrix = {
    val nz = new ArrayBuffer[Array[Int]]()
    for(i <- matrix.indices) {
      val row = new ArrayBuffer[Int]()
      for(j <- matrix.indices; if matrix(i)(j) != 0) {
        row += j
      }
      nz += row.toArray
    }
    val sparseMatrix = new CompRowMatrix(matrix.length, matrix.length, nz.toArray)

    for(i <- matrix.indices; j <- matrix.indices if matrix(i)(j) != 0) {
      sparseMatrix.set(i, j, matrix(i)(j))
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
