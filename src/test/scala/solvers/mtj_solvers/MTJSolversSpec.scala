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

  it should "use CGS to give the steady state distribution of a 4-state ergodic CTMC" in {
    val solver = new CGS(new DenseVector(Array.fill(4)(0.25)))
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
    val solution = solver.solve(VALID_4x4_MC._1.transpose(), new DenseVector(Array.ofDim[Double](4)), new DenseVector(Array.ofDim[Double](4)))
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
