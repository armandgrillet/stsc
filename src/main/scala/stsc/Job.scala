package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

object Job {
    def main(args: Array[String]) {
        // val conf = new SparkConf().setAppName("STSCSparkJob")
        // val sc = new SparkContext(conf)
        // sc.addJar("/Users/Armand/Code/stsc/target/scala-2.11/stsc-assembly-1.0.jar")
        // STSC.sparkCluster(sc, args(0), args(1), "clusters")
        // STSC.sparkCluster(sc, args(0), args(1), "clusterCenters.csv", true)

        val dataPath = getClass.getResource("/dataset.csv").getPath()
        val t0 = System.nanoTime()
        val (bestK, clustersQualities, correctClusters) = STSC.clusterCSV(dataPath, 40, 60)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) / 1000000000 + "s")
        println(bestK)
        println(clustersQualities)
        // STSC.sparkCluster(sc, args(0), args(1), "clusters")
        // STSC.sparkCluster(sc, args(0), args(1), "clusterCenters.csv", true)
        //new File("clusters").delete()
        // sc.stop()
    }
}
