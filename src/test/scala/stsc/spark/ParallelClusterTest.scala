package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

import org.scalatest.{FlatSpec, Matchers}

import java.io.File

class ParallelClusterTest extends FlatSpec with Matchers {
    "Clustering in parallel a small dataset" should "work" in {
        val conf = new SparkConf().setAppName("ParallelClusterTest").setMaster("spark://main:7077")
        val sc = new SparkContext(conf)
        sc.addJar("/home/Armand/stsc/target/scala-2.11/stsc-assembly-1.0.jar")

        val dataPath = getClass.getResource("/datasetforkdt1.csv").getPath()
        val ttPath = getClass.getResource("/kdt1.csv").getPath()
        STSC.sparkCluster(sc, dataPath, ttPath, "clusters")
        STSC.sparkCluster(sc, dataPath, ttPath, "clusterCenters.csv", true)
        sc.stop()
    }

    "Clustering in parallel a dataset with 1000 clusters" should "work" in {
        val t0 = System.nanoTime()
        val conf = new SparkConf().setAppName("STSCSparkJob").setMaster("spark://main:7077")
        val sc = new SparkContext(conf)
        val dataset = getClass.getResource("/dataset1000.csv").getPath()
        val kdtree = getClass.getResource("/kdt1000.csv").getPath()
        sc.addJar("/home/Armand/stsc/target/scala-2.11/stsc-assembly-1.0.jar")
        // STSC.sparkCluster(sc, dataset, kdtree, "clusters")
        STSC.sparkCluster(sc, dataset, kdtree, "clusterCenters.csv", true, 2, 20)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        sc.stop()
    }
}
