package solvers.mtj_solvers

import no.uib.cipr.matrix.{DenseMatrix, DenseVector, Vector}
import no.uib.cipr.matrix.sparse._
import org.scalatest.{FlatSpec, Matchers}

class MTJSolversSpec extends FlatSpec with Matchers{
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

  s"Solvers in matrix toolkis java library" should "use BiCG to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new BiCG(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use BiCGstab to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new BiCGstab(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use CG to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new CG(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use CGS to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new CGS(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use Chebysev to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new Chebyshev(new DenseVector(Array.fill(4)(0.25)), 1, 1000)
    countAndCheck(solver)
  }

  it should "use GMRES to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new GMRES(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use IR to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new IR(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  it should "use QMR to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new QMR(new DenseVector(Array.fill(4)(0.25)))
    countAndCheck(solver)
  }

  private def countAndCheck(solver: AbstractIterativeSolver) = {
    val solution = solver.solve(VALID_4x4_MC._1, new DenseVector(Array.ofDim[Double](4)), new DenseVector(Array.ofDim[Double](4)))
    println(solution.toString)
    norm(solution.add(VALID_4x4_MC._2.scale(-1))) shouldBe <= (1e-10)
  }

  private def norm(v: Vector): Double = {
    var norm = 0.0
    for (i <- 0 until v.size()) {
      norm += v.get(i) * v.get(i)
    }
    Math.sqrt(norm)
  }
}
