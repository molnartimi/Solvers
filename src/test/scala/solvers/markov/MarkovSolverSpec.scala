package solvers.markov

import generated.MatrixFactory
import no.uib.cipr.matrix
import no.uib.cipr.matrix.DenseVector
import org.scalatest.{FlatSpec, Matchers}

class MarkovSolverSpec extends FlatSpec with Matchers {
  val tol = 1e-8

  def test(solverType: SolverAlgorithmConfig, preconditionerType: PreconditionerConfig, testMatrixDim: Int): Unit = {
    val config = new SolverConfig(solverType, preconditionerType, ToleranceConfig.default)
    val testData = MatrixFactory.makeBinaryCRM(testMatrixDim)

    val ctmc = new CTMC(testData._1)

    val solver = new MarkovSolver(ctmc, config, "testlog1.csv")
    val Right(solution) = solver.solve

    differenceBetween(solution.asInstanceOf[DenseVector], testData._2) shouldBe <=(tol)
  }

  def differenceBetween(counted: matrix.Vector, expected: DenseVector): Double = {
    val difference = counted.add(-1, expected)
    difference.norm(matrix.Vector.Norm.Two)
  }

  val TEST_DIM = 10000

  "Our continuous time markov chain solver" should "give the steady state distribution of a 1000-state CTMC " +
    "with BiCG preconditioner" in {
    test(BiCG, Diagonal, TEST_DIM)
  }

  it should "give the steady state distribution " +
    "with BiCGStab preconditioner" in {
    test(BiCGStab, Diagonal, TEST_DIM)
  }

  it should "give the steady state distribution " +
    "with CGS preconditioner" in {
    test(CGS, Diagonal, TEST_DIM)
  }

  it should "give the steady state distribution " +
    "with GMRES preconditioner" in {
    test(GMRES(), Diagonal, TEST_DIM)
  }

  it should "give the steady state distribution " +
    "with IR preconditioner" in {
    test(IR, Diagonal, TEST_DIM)
  }

  it should "give the steady state distribution " +
    "with QMR preconditioner" in {
    test(QMR, Diagonal, TEST_DIM)
  }

}