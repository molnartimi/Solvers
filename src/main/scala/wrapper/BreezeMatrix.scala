package wrapper

import breeze.linalg.DenseMatrix

class BreezeMatrix(value: Array[Array[Double]]) extends Matrix(value) {
  protected var matrix = {
    val row = value.length
    val col = value(0).length
    val array = Array.ofDim[Double](row * col)
    for(i <- 0 until row; j <- 0 until col) {
      array(i * row + j) = value(j)(i)
    }
    new DenseMatrix(row, col, array)
  }

  override def rows(): Int = matrix.rows

  override def cols(): Int = matrix.cols

  override def apply(x: Int, y: Int): Double = matrix(x, y)

  override def set(x: Int, y: Int, value: Double): Unit = { matrix(x, y) = value }

  override def copy(): Matrix = create(matrix.copy)

  protected def create(m: DenseMatrix[Double]): BreezeMatrix = {
    val newMatrix = new BreezeMatrix(Array.ofDim(m.rows, m.cols))
    newMatrix.matrix = m
    newMatrix
  }
}
