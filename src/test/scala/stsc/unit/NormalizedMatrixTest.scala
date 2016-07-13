package stsc

import breeze.linalg.{DenseMatrix, DenseVector, argmax, csvwrite, diag, upperTriangular, lowerTriangular, eigSym, max, sum, *}
import breeze.numerics.{abs, cos, pow, sin, sqrt}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class NormalizedMatrixTest extends FlatSpec with Matchers {
    "The normalized matrix" should "work" in {
        // val dataset = new File(getClass.getResource("/simpleScaledMatrix.csv").getPath())
        // val scaledMatrix = breeze.linalg.csvread(dataset)
        // val diagScaledMatrix = diag(pow(sum(scaledMatrix(*, ::)), -0.5)) // Sum of each row, then power -0.5, then matrix.
        // println(diagScaledMatrix)
        // var normalizedMatrixOne = diagScaledMatrix * scaledMatrix * diagScaledMatrix
        //
        // // var row, col = 0
        // // for (row <- 0 until normalizedMatrixOne.rows) {
        // //     for (col <- row + 1 until normalizedMatrixOne.cols) {
        // //         normalizedMatrixOne(col, row) = normalizedMatrixOne(row, col)
        // //     }
        // // }
        //
        // println(normalizedMatrixOne)
        //
        // val diagonalVector = DenseVector.tabulate(scaledMatrix.rows){i => 1 / sqrt(sum(scaledMatrix(i, ::))) }
        // var normalizedMatrixTwo = DenseMatrix.zeros[Double](scaledMatrix.rows, scaledMatrix.cols)
        //
        // for (row <- 0 until normalizedMatrixTwo.rows) {
        //     for (col <- row + 1 until normalizedMatrixTwo.cols) {
        //         normalizedMatrixTwo(row, col) = diagonalVector(col) * scaledMatrix(row, col) * diagonalVector(row)
        //         normalizedMatrixTwo(col, row) = normalizedMatrixTwo(row, col)
        //     }
        // }
        //
        // normalizedMatrixOne should be (normalizedMatrixTwo)
        //
        // val matrix = DenseMatrix((1.0,2.0), (3.0,4.0))
        // println(pow(matrix, -0.5))
    }
}
