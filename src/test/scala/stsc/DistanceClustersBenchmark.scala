package stsc

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}

import org.scalameter._
import org.scalatest.FunSuite

class DistanceClustersBenchmark extends FunSuite {
    def keyForMaxValueWithMargin(qualities: Map[Int, Double]): Int = {
        var maxValue = 0.0
        var keyForMaxValue = 0
        for ((k, v) <- qualities) {
            if (v > maxValue) {
                maxValue = v
                keyForMaxValue = k
            } else if (maxValue - 0.002 < v) {
                keyForMaxValue = k
            }
        }
        return keyForMaxValue
    }

    test("Should work with 2 clusters of 100 observations in 1 dimension") {
        val time = measure {
            var result = 2
            var distance = 9
            while (result == 2) {
                val sample1 = Gaussian(0, 1).sample(100)
                val sample2 = Gaussian(distance, 1).sample(100)
                val samplesMatrix = DenseMatrix.zeros[Double](sample1.length * 2, 1)
                samplesMatrix(::, 0) := DenseVector((sample1 ++ sample2).toArray)
                result = keyForMaxValueWithMargin(Algorithm.cluster(samplesMatrix)._1)

                println(result)
                println(distance)
                distance = distance - 1
            }
        }
        println("Total time : " + time)
    }
}
