package wrapper

object VectorFactory {
  def create(value: Array[Double]): Vector = new BreezeVector(value)
}
