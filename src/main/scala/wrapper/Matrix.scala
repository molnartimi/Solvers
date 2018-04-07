package wrapper

abstract class Matrix(value: Array[Array[Double]]) {
  def rows(): Int
  def cols(): Int
  def apply(x: Int, y: Int): Double
  def set(x: Int, y: Int, value: Double)
}
