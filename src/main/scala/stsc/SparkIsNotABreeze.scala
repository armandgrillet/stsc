package stsc

import breeze.linalg.{Axis, DenseMatrix, DenseVector, Transpose, sum}
import scala.math.abs

// All the functions that work with Breeze 0.12 but, as Spark does whatever it wants with the dependencies, need to be reimplemented.
object SparkIsNotABreeze {
    // Sum the columns to only have one DenseVector at the end.
    def sumCols(dm: DenseMatrix[Double]): DenseVector[Double] = {
        return DenseVector.tabulate(dm.rows){ i => sum(dm(i, ::))}
    }

    // Sum the rows to only have one Transpose at the end.
    def sumRows(dm: DenseMatrix[Double]): Transpose[DenseVector[Double]] = {
        return DenseVector.tabulate(dm.cols){ i => sum(dm(::, i))}.t
    }

    // Absolute version of the DenseMatrix.
    def getSomeAbs(pullIn: DenseMatrix[Double]): DenseMatrix[Double] = {
        return DenseMatrix.tabulate(pullIn.rows, pullIn.cols){ case (i, j) => abs(pullIn(i, j)) }
    }
}
