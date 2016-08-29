package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

import org.scalatest.{FlatSpec, Matchers}

import java.io.File

class ClusterTimeTest extends FlatSpec with Matchers {
    "The sequential test" should "work" in {
        val dataPath = getClass.getResource("/dataset.csv").getPath()
        val t0 = System.nanoTime()
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath, 30, 40)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) / 1000000000 + "s")
        println(bestK)
        println(clustersQualities)
        println(correctClusters)
    }

    "The parallel test" should "work" in {
        val t0 = System.nanoTime()
        val conf = new SparkConf().setAppName("STSCSparkJob").setMaster("spark://Mac:7077")
        val sc = new SparkContext(conf)
        val dataset = getClass.getResource("/dataset.csv").getPath()
        val kdtree = getClass.getResource("/kdtree.csv").getPath()
        sc.addJar("/Users/Armand/Code/stsc/target/scala-2.11/stsc-assembly-1.0.jar")
        // STSC.sparkCluster(sc, dataset, kdtree, "clusters")
        STSC.sparkCluster(sc, dataset, kdtree, "clusterCenters.csv", true, 2, 12)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        sc.stop()
    }
}
