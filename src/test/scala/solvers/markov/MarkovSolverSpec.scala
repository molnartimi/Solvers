package solvers.markov

import generated.MatrixFactory
import no.uib.cipr.matrix
import no.uib.cipr.matrix.DenseVector
import org.scalatest.{FlatSpec, Matchers}

class MarkovSolverSpec extends FlatSpec with Matchers {
  val tol = 1e-8
  val TEST_DIM = 100000
  val preconditioner = Diagonal(TEST_DIM)
  val LOG_FILE = s"log/log${TEST_DIM}.csv"

  def test(solverType: SolverAlgorithmConfig): Unit = {
    val config = new SolverConfig(solverType, preconditioner, ToleranceConfig.default)
    val testData = MatrixFactory.makeBinary(TEST_DIM)

    val ctmc = new CTMC(testData._1)

    val solver = new MarkovSolver(ctmc, config, LOG_FILE)
    val Right(solution) = solver.solve

    differenceBetween(solution.asInstanceOf[DenseVector], testData._2) shouldBe <=(tol)
  }

  def differenceBetween(counted: matrix.Vector, expected: DenseVector): Double = {
    val difference = counted.add(-1, expected)
    difference.norm(matrix.Vector.Norm.Two)
  }

  "Our continuous time markov chain solver" should "give the steady state distribution of a 1000-state CTMC " +
    "with BiCG solver" in {
    test(BiCG)
  }

  it should "give the steady state distribution " +
    "with BiCGStab solver" in {
    test(BiCGStab)
  }

  it should "give the steady state distribution " +
    "with CGS solver" in {
    test(CGS)
  }

  it should "give the steady state distribution " +
    "with GMRES solver" in {
    test(GMRES())
  }

  it should "give the steady state distribution " +
    "with IR solver" in {
    test(IR)
  }

  it should "give the steady state distribution " +
    "with QMR solver" in {
    test(QMR)
  }

}