package wrapper

abstract class Vector(value: Array[Double]) {
  def size(): Int
  def apply(x: Int): Double
  def set(x: Int, value: Double)
  def +(v: Vector): Vector
  def -(v: Vector): Vector
  def *(num: Double): Vector
  def /(num: Double): Vector
}