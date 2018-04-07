package wrapper

object MatrixType extends Enumeration {
  type MatrixType = Value
  val BREEZE = Value
}

object MatrixFactory {
  def create(matrixType: wrapper.MatrixType.MatrixType, value: Array[Array[Double]]): Matrix = {
    matrixType match {
      case wrapper.MatrixType.BREEZE => new BreezeMatrix(value)
      case _ => new BreezeMatrix(value)
    }
  }
}
