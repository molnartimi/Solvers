package solvers

import no.uib.cipr.matrix.{DenseMatrix, DenseVector, Vector}
import no.uib.cipr.matrix.sparse.LinkedSparseMatrix
import org.scalatest.{FlatSpec, Matchers}

class SparseSolverTest(solver: SparseSolver, name: String) extends FlatSpec with Matchers{
  val VALID_4x4_MC = (
    new LinkedSparseMatrix(new DenseMatrix(Array(
      Array(-4.0, 1.0, 2.0, 1.0),
      Array(4.0, -9.0, 2.0, 3.0),
      Array(0.0, 1.0, -3.0, 2.0),
      Array(0.0, 0.0, 5.0, -5.0)
    ))),
    new DenseVector(Array(0.25/3.5, 0.25/3.5, 2/3.5, 1/3.5))
  )

  val INVALID_2x3_MC  = new LinkedSparseMatrix(new DenseMatrix(Array(
    Array(-1.0, 1.0, 0.0),
    Array(0.1, -0.5, 0.4))
  ))

  val INVALID_3x2_MC = new LinkedSparseMatrix(new DenseMatrix(Array(
    Array(-1.0, 1.0),
    Array(3.0, -3.0),
    Array(5.0, 6.0))
  ))

  val INVALID_ROW_SUM_NOT_0_3x3_MC = new LinkedSparseMatrix(new DenseMatrix(Array(
    Array(7.0, 100.0, 1.0),
    Array(2.0, 2.0, 3.0),
    Array(1000.0, 0.0, 1.0))
  ))

  val INVALID_NEG_NUMS_3x3_MC = new LinkedSparseMatrix(new DenseMatrix(Array(
    Array(7.0, -4.0, -3.0),
    Array(1.0, -3.0, 2.0),
    Array(9.0, -10.0, 1.0))
  ))

  val INVALID_REDUCIBLE_4x4_MC = new LinkedSparseMatrix(new DenseMatrix(Array(
    Array(-0.5, 0.0, 0.5, 0.0),
    Array(0.0, -0.5, 0.0, 0.5),
    Array(0.5, 0.0, -0.5, 0.0),
    Array(0.0, 0.5, 0.0, -5.0))
  ))

  s"Our solver with $name" should "give the steady state distribution of a 4-state ergodic CTMC" in {
    val Right(solution: DenseVector) = solver.solveSteadyState(VALID_4x4_MC._1)
    norm(solution.add(VALID_4x4_MC._2.scale(-1))) shouldBe <= (1e-10)
  }

  it should "give NotAMarkovChain answer if the matrix has more columns than rows" in {
    val Left(solution1) = solver.solveSteadyState(INVALID_2x3_MC)
    solution1 shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix has more rows than columns" in {
    val Left(solution2) = solver.solveSteadyState(INVALID_3x2_MC)
    solution2 shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix is not a valid Markov Chain" in {
    val Left(solution) = solver.solveSteadyState(INVALID_ROW_SUM_NOT_0_3x3_MC)
    solution shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix is not a valid Markov Chain OTHER EXAMPLE" in {
    val Left(solution) = solver.solveSteadyState(INVALID_NEG_NUMS_3x3_MC)
    solution shouldBe NotAMarkovChain
  }

  private def norm(v: Vector): Double = {
    var norm = 0.0
    for (i <- 0 until v.size()) {
      norm += v.get(i) * v.get(i)
    }
    Math.sqrt(norm)
  }
}
