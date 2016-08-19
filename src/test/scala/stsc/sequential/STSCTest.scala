package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class STSCTest extends FlatSpec with Matchers {
    def compressDenseVector(dv: Array[Int], values: Int): DenseVector[Int] = {
        val differentValues = DenseVector.zeros[Int](values)
        var count = 0
        for (i <- 0 until dv.length) {
            if (i > 0 && dv(i-1) != dv(i)) {
                count += 1
            }
            differentValues(count) += 1
        }
        return differentValues
    }

    // Unit tests/
    "The algorithm" should "work when the minimum and maximum number of clusters are the same" in {
        val dataPath = getClass.getResource("/near.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath, 2, 2)

        bestK should be (2)
        correctClusters should not be DenseVector.zeros[Int](matrix.rows)
        correctClusters should not be DenseVector.ones[Int](matrix.rows)
    }

    // Global tests.
    "The dataset 0" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (3)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(61, 139, 99))
    }

    "The dataset 1" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/1.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (3)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(106, 102, 95))
    }

    "The dataset 2" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/2.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (3)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(118, 75, 73))
    }

    "The dataset 3" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/3.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (5)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(136, 116, 111, 150, 109))
    }

    "The dataset 4" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/4.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (4)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(117, 123, 150, 122))
    }

    "The dataset 5" should "be correctly clustered" in {
        val dataPath = getClass.getResource("/5.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath)
        println(clustersQualities)
        bestK should be (3)
        compressDenseVector(correctClusters, bestK) should be (DenseVector(56, 82, 100))
    }
}
