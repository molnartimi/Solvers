package solvers.origin_solvers

import generated.MatrixFactory
import no.uib.cipr.matrix.sparse._
import no.uib.cipr.matrix.{DenseMatrix, DenseVector, Matrix, Vector, ZeroVector}
import org.scalatest.{FlatSpec, Matchers}

abstract class MTJSolversSpec(name: String) extends FlatSpec with Matchers{
  val VALID_4x4_MC = (
    new LinkedSparseMatrix(new DenseMatrix(Array(
      Array(-4.0, 1.0, 2.0, 1.0),
      Array(4.0, -9.0, 2.0, 3.0),
      Array(0.0, 1.0, -3.0, 2.0),
      Array(0.0, 0.0, 5.0, -5.0)
    ))),
    new DenseVector(Array(0.25/3.5, 0.25/3.5, 2/3.5, 1/3.5))
  )

  // A*x = b  plus the init vector
  val NOT_MARKOV_EXAMPLE_3x3 = (
    new LinkedSparseMatrix(new DenseMatrix(Array(
      Array(1.0, 2.0, 3.0),
      Array(2.0, 1.0, 1.0),
      Array(3.0, 2.0, 3.0)
    ))),
    new DenseVector(Array(2.0, 3.0, 1.0)),
    new DenseVector(Array(11.0, 8.0, 15.0)),
    new DenseVector(Array(5.0, 5.0, 5.0))
  )

  protected val GEN_SIZE = 4
  protected val GENERATED = MatrixFactory.makeBinary(GEN_SIZE)

  protected val preconditioned = true

  s"$name solver in matrix toolkis java library" should "give the steady state distribution of a 4-state ergodic CTMC" in {
    test(VALID_4x4_MC._1, VALID_4x4_MC._2)
  }

  it should "give the steady state distribution of a generated ergodic CTMC" in {
    test(GENERATED._1, GENERATED._2)
  }

  it should "give the solution of a not MC matrix and vector" in {
    test(NOT_MARKOV_EXAMPLE_3x3._1, NOT_MARKOV_EXAMPLE_3x3._2, NOT_MARKOV_EXAMPLE_3x3._3, NOT_MARKOV_EXAMPLE_3x3._4, true)
  }

  protected def test(matrix: Matrix, solution: Vector): Unit = {
    val dim = matrix.numColumns()
    test(matrix, solution, new ZeroVector(dim), getInitVector(dim), false)
  }

  protected def test(matrix:Matrix, realSolution: Vector, right: Vector, init: Vector, origin: Boolean): Unit = {
    val solver = if (origin) createOriginSolver(init) else createMySolver(init)
    if (preconditioned)
      setPreconditioner(solver, matrix)
    val solution = solver.solve(matrix, right, init)
    norm(solution.add(realSolution.scale(-1))) shouldBe <= (1e-8)
  }

  protected def createMySolver(template: Vector): AbstractIterativeSolver

  protected def createOriginSolver(template: Vector): AbstractIterativeSolver

  protected def setPreconditioner(solver: AbstractIterativeSolver, matrix: Matrix): Unit =  {
    val M: Preconditioner = new DiagonalPreconditioner(matrix.numColumns());
    M.setMatrix(matrix)
    solver.setPreconditioner(M)
  }

  protected def getInitVector(dim: Int): DenseVector = {
    new DenseVector(Array.fill[Double](dim)(1.0/dim));
  }

  protected def norm(v: Vector): Double = {
    var norm = 0.0
    for (i <- 0 until v.size()) {
      norm += v.get(i) * v.get(i)
    }
    Math.sqrt(norm)
  }
}
