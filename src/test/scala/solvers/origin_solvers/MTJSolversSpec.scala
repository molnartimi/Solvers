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

  protected val preconditioned = true

  s"$name solver in matrix toolkis java library" should "give the steady state distribution of a 4-state ergodic CTMC" in {
    test(VALID_4x4_MC._1, VALID_4x4_MC._2)
  }

  it should "give the steady state distribution of a  5x5 generated ergodic CTMC" in {
    val task = MatrixFactory.makeBinary(5)
    test(task._1, task._2)
  }

  it should "give the steady state distribution of a  50x50 generated ergodic CTMC" in {
    val task = MatrixFactory.makeBinary(50)
    test(task._1, task._2)
  }

  it should "give the steady state distribution of a  500x500 generated ergodic CTMC" in {
    val task = MatrixFactory.makeBinary(500)
    test(task._1, task._2)
  }

  it should "give the steady state distribution of a  5000x5000 generated ergodic CTMC" in {
    val task = MatrixFactory.makeBinary(5000)
    test(task._1, task._2)
  }

  protected def test(matrix: Matrix, solution: Vector): Unit = {
    val dim = matrix.numColumns()
    test(matrix, solution, new ZeroVector(dim), getInitVector(dim))
  }

  protected def test(matrix:Matrix, realSolution: Vector, right: Vector, init: Vector): Unit = {
    val solver = createSolver(init)
    if (preconditioned)
      setConditioners(solver, matrix)
    setIterMonitor(solver)
    val solution = solver.solve(matrix.transpose(), right, init)
    norm(solution.add(realSolution.scale(-1))) shouldBe <= (1e-6)
  }

  protected def setIterMonitor(solver: AbstractIterativeSolver): Unit = {
    solver.setIterationMonitor(new DefaultIterationMonitor(200, 0, 1e-8, 10))
  }

  protected def createSolver(template: Vector): AbstractIterativeSolver

  protected def setConditioners(solver: AbstractIterativeSolver, matrix: Matrix): Unit =  {
    val M: Preconditioner = new DiagonalPreconditioner(matrix.numColumns())
    M.setMatrix(matrix)
    solver.setPreconditioner(M)
    solver.setNormalizer(new OneNormNormalizer())
  }

  protected def getInitVector(dim: Int): DenseVector = {
    new DenseVector(Array.fill[Double](dim)(1.0/dim))
  }

  protected def norm(v: Vector): Double = {
    var norm = 0.0
    for (i <- 0 until v.size()) {
      norm += v.get(i) * v.get(i)
    }
    Math.sqrt(norm)
  }
}
