package generated

import org.scalatest.{FlatSpec, Matchers}

class MatrixTest extends FlatSpec with Matchers{
  "MatrixFactory" should " create big generated MC matrix" in {
    val (matrix, vector) = MatrixFactory.makeBinaryLSM(4)
    println(matrix.toString)
    println(vector.toString)
  }
}
