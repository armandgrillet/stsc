package stsc

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}

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

    // test("Should work with 2 clusters of 1000 observations in 1 dimension") {
    //     val sample1 = Gaussian(0, 1).sample(1000)
    //     val sample2 = Gaussian(10, 1).sample(1000)
    //     val samplesMatrix = DenseMatrix.zeros[Double](sample1.length * 2, 1)
    //     samplesMatrix(::, 0) := DenseVector((sample1 ++ sample2).toArray)
    //     val time = measure {
    //         val result = Algorithm.cluster(samplesMatrix)
    //         println(result._1)
    //     }
    //     println("Total time : " + time)
    // }

    test("Should work with 2 clusters of 100 observations in 2 dimensions") {
        val sample1Vector = new MultivariateGaussian(DenseVector(0.0, 0.0),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(100)
        val sample2Vector = new MultivariateGaussian(DenseVector(0.0, 6.0),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(100)

        val sample1 = DenseMatrix.zeros[Double](100, 2)
        val sample2 = DenseMatrix.zeros[Double](100, 2)

        var i = 0
        for (i <- 0 until sample1Vector.length){
            sample1(i, 0) = sample1Vector(i)(0)
            sample1(i, 1) = sample1Vector(i)(1)

            sample2(i, 0) = sample2Vector(i)(0)
            sample2(i, 1) = sample2Vector(i)(1)
        }

        val samplesMatrix = DenseMatrix.vertcat(sample1, sample2)
        val time = measure {
            val result = Algorithm.cluster(samplesMatrix)
            println(result._1)
        }
        println("Total time : " + time)
    }

    // test("Should work with 2 clusters of 1000 observations in 2 dimensions") {
    //     val sample1Vector = new MultivariateGaussian(DenseVector(0.0, 0.0),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(100)
    //     val sample2Vector = new MultivariateGaussian(DenseVector(0.0, 6.0),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(100)
    //
    //     val sample1 = DenseMatrix.zeros[Double](1000, 2)
    //     val sample2 = DenseMatrix.zeros[Double](1000, 2)
    //
    //     var i = 0
    //     for (i <- 0 until sample1Vector.length){
    //         sample1(i, 0) = sample1Vector(i)(0)
    //         sample1(i, 1) = sample1Vector(i)(1)
    //
    //         sample2(i, 0) = sample2Vector(i)(0)
    //         sample2(i, 1) = sample2Vector(i)(1)
    //     }
    //
    //     val samplesMatrix = DenseMatrix.vertcat(sample1, sample2)
    //     val time = measure {
    //         val result = Algorithm.cluster(samplesMatrix)
    //         println(result._1)
    //     }
    //     println("Total time : " + time)
    // }
}
