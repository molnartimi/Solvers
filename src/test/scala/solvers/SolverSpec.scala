package solvers

import generated.MatrixFactory
import wrapper.Matrix
import org.scalatest.{FlatSpec, Matchers}

abstract class SolverSpec(solver: Solver, matrixType: wrapper.MatrixType.MatrixType, name: String) extends FlatSpec with Matchers{
  val VALID_4x4_MC = (
    Array(
    Array(-4.0, 1.0, 2.0, 1.0),
    Array(4.0, -9.0, 2.0, 3.0),
    Array(0.0, 1.0, -3.0, 2.0),
    Array(0.0, 0.0, 5.0, -5.0)
    ),
    Array(0.25/3.5, 0.25/3.5, 2/3.5, 1/3.5)
  )

  val INVALID_2x3_MC  = Array(
    Array(-1.0, 1.0, 0.0),
    Array(0.1, -0.5, 0.4)
  )

  val INVALID_3x2_MC = Array(
    Array(-1.0, 1.0),
    Array(3.0, -3.0),
    Array(5.0, 6.0)
  )

  val INVALID_ROW_SUM_NOT_0_3x3_MC = Array(
    Array(7.0, 100.0, 1.0),
    Array(2.0, 2.0, 3.0),
    Array(1000.0, 0.0, 1.0)
  )

  val INVALID_NEG_NUMS_3x3_MC = Array(
    Array(7.0, -4.0, -3.0),
    Array(1.0, -3.0, 2.0),
    Array(9.0, -10.0, 1.0)
  )

  val INVALID_REDUCIBLE_4x4_MC = Array(
    Array(-0.5, 0.0, 0.5, 0.0),
    Array(0.0, -0.5, 0.0, 0.5),
    Array(0.5, 0.0, -0.5, 0.0),
    Array(0.0, 0.5, 0.0, -5.0)
  )

  s"Our solver with $name" should "give the steady state distribution of a 4-state ergodic CTMC" in {
    val Right(solution) = solver.solveSteadyState(wrapper.MatrixFactory.create(matrixType, VALID_4x4_MC._1))
    (solution - wrapper.VectorFactory.create(VALID_4x4_MC._2)).norm() shouldBe <= (1e-10)
  }

  it should "give NotAMarkovChain answer if the matrix has more columns than rows" in {
    val Left(solution) = solver.solveSteadyState(wrapper.MatrixFactory.create(matrixType, INVALID_2x3_MC))
    solution shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix has more rows than columns" in {
    val Left(solution) = solver.solveSteadyState(wrapper.MatrixFactory.create(matrixType, INVALID_3x2_MC))
    solution shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix is not a valid Markov Chain" in {
    val Left(solution) = solver.solveSteadyState(wrapper.MatrixFactory.create(matrixType, INVALID_ROW_SUM_NOT_0_3x3_MC))
    solution shouldBe NotAMarkovChain
  }

  it should "give NotAMarkovChain answer if the matrix is not a valid Markov Chain OTHER EXAMPLE" in {
    val Left(solution) = solver.solveSteadyState(wrapper.MatrixFactory.create(matrixType, INVALID_NEG_NUMS_3x3_MC))
    solution shouldBe NotAMarkovChain
  }

  /*ignore should("count a big generated matrix - binary") in {
    for (i <- 3 to 100) {
      val Right(solution) = solver.solveSteadyState(MatrixFactory.makeBinary(i))
      println(solution.toString())
    }
  }

  ignore should("count a big generated matrix - simple") in {
    for (i <- 3 to 100) {
      val Right(solution) = solver.solveSteadyState(MatrixFactory.makeSimple(i))
      println(solution.toString())
    }
  }*/
}
