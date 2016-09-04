package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

import org.scalatest.{FlatSpec, Matchers}

import java.io.File

class SequentialParallelComparisonTest extends FlatSpec with Matchers {
    "Clustering sequentially a dataset with 36 clusters" should "work" in {
        val dataPath = getClass.getResource("/dataset36.csv").getPath()
        val t0 = System.nanoTime()
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath, 30, 40)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) / 1000000000 + "s")
        println(bestK)
        println(clustersQualities)
        println(correctClusters)
    }

    "Clustering in parallel a dataset with 36 clusters" should "work" in {
        val t0 = System.nanoTime()
        val conf = new SparkConf().setAppName("STSCSparkJob").setMaster("spark://main:7077")
        val sc = new SparkContext(conf)
        val dataset = getClass.getResource("/dataset36.csv").getPath()
        val kdtree = getClass.getResource("/kdt36.csv").getPath()
        sc.addJar("/home/Armand/stsc/target/scala-2.11/stsc-assembly-1.0.jar")
        // STSC.sparkCluster(sc, dataset, kdtree, "clusters")
        STSC.sparkCluster(sc, dataset, kdtree, "clusterCenters.csv", true, 2, 20)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        sc.stop()
    }
}
