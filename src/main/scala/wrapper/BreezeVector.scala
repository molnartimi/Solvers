package wrapper

import breeze.linalg.{DenseVector}

class BreezeVector(value: Array[Double]) extends Vector(value) {
  protected var vector = new DenseVector[Double](value)

  override def size(): Int = vector.length

  override def apply(x: Int): Double = vector(x)

  override def set(x: Int, value: Double): Unit = { vector(x) = value }

  override def +(v: Vector): Vector = create(vector + v.vector)

  override def -(v: Vector): Vector = create(vector - v.vector)

  override def *(num: Double): Vector = create(vector * num)

  override def /(num: Double): Vector = create(vector / num)

  override def sum(): Double = breeze.linalg.sum(vector)

  override def norm(): Double = breeze.linalg.norm(vector)

  protected def create(v: DenseVector[Double]): BreezeVector = {
    val newVec = new BreezeVector(Array.emptyDoubleArray)
    newVec.vector = v
    newVec
  }
}
