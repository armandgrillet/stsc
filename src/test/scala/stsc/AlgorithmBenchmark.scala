package stsc

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian

import org.scalameter._
import org.scalatest.FunSuite

class AlgorithmBenchmark extends FunSuite {
    test("Should work with 2 clusters of 100 observations in 1 dimension") {
        val sample1 = Gaussian(0, 1).sample(100)
        val sample2 = Gaussian(5, 1).sample(100)
        val samplesMatrix = DenseMatrix.zeros[Double](sample1.length * 2, 1)
        samplesMatrix(::, 0) := DenseVector((sample1 ++ sample2).toArray)
        val time = measure {
            val result = Algorithm.cluster(samplesMatrix)
            println(result._1)
        }
        println("Total time : " + time)
    }
}
