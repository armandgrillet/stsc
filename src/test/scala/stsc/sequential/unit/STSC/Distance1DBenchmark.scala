package stsc

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}

import org.scalameter._
import org.scalatest.FunSuite

class Distance1DBenchmark extends FunSuite {
    def compressDenseVector(dv: Array[Int], values: Int): DenseVector[Int] = {
        val differentValues = DenseVector.zeros[Int](values)
        var count = 0
        for (i <- 0 until dv.length) {
            if (i > 0 && dv(i-1) != dv(i)) {
                if (count < differentValues.length - 1) {
                    count += 1
                }
            }
            differentValues(count) += 1
        }
        return differentValues
    }

    test("Should work with 2 clusters of 100 observations in 1 dimension") {
        val time = measure {
            var z = DenseVector(100, 100)
            var distance = 9.0
            while (z == DenseVector(100, 100)) {
                val sample1 = Gaussian(0, 1).sample(100)
                val sample2 = Gaussian(distance, 1).sample(100)
                val samplesMatrix = DenseMatrix.zeros[Double](sample1.length * 2, 1)
                samplesMatrix(::, 0) := DenseVector((sample1 ++ sample2).toArray)

                z = compressDenseVector(STSC.cluster(samplesMatrix)._3, 2)
                println(distance)
                distance -= 1
            }
        }
        println("Total time : " + time)
    }
}
