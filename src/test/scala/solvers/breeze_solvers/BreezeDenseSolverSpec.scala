package solvers.breeze_solvers

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import generated.MatrixFactory
import org.scalatest.{FlatSpec, Matchers}
import solvers.NotAMarkovChain

abstract class BreezeDenseSolverSpec(solver: BreezeDenseSolver, name: String) extends FlatSpec with Matchers{
  val VALID_4x4_MC = (
    DenseMatrix(
    (-4.0, 1.0, 2.0, 1.0),
    (4.0, -9.0, 2.0, 3.0),
    (0.0, 1.0, -3.0, 2.0),
    (0.0, 0.0, 5.0, -5.0)
    ),
    DenseVector(0.25, 0.25, 2, 1) / 3.5
  )

  val INVALID_2x3_MC  = DenseMatrix(
    (-1.0, 1.0, 0.0),
    (0.1, -0.5, 0.4)
  )

  val INVALID_3x2_MC = DenseMatrix(
    (-1.0, 1.0),
    (3.0, -3.0),
    (5.0, 6.0)
  )

  val INVALID_ROW_SUM_NOT_0_3x3_MC = DenseMatrix(
    (7.0, 100.0, 1.0),
    (2.0, 2.0, 3.0),
    (1000.0, 0.0, 1.0)
  )

  val INVALID_NEG_NUMS_3x3_MC = DenseMatrix(
    (7.0, -4.0, -3.0),
    (1.0, -3.0, 2.0),
    (9.0, -10.0, 1.0)
  )

  val INVALID_REDUCIBLE_4x4_MC = DenseMatrix(
    (-0.5, 0.0, 0.5, 0.0),
    (0.0, -0.5, 0.0, 0.5),
    (0.5, 0.0, -0.5, 0.0),
    (0.0, 0.5, 0.0, -5.0)
  )

  s"Our solver with $name" should "give the steady state distribution of a 4-state ergodic CTMC" in {
    val Right(solution) = solver.solveSteadyState(VALID_4x4_MC._1)
    norm(solution - VALID_4x4_MC._2) shouldBe <= (1e-10)
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

  it should "count a lot of big generated matrix - binary" in {
    for (i <- 3 to 100) {
      val Right(solution) = solver.solveSteadyState(MatrixFactory.makeBreezeBinary(i))
      println(solution.toString())
    }
  }

  it should "count a lot of big generated matrix - simple" in {
    for (i <- 3 to 100) {
      val Right(solution) = solver.solveSteadyState(MatrixFactory.makeBreezeSimple(i))
      println(solution.toString())
    }
  }
}
