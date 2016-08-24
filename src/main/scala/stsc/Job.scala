package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

object Job {
    def main(args: Array[String]) {
        val conf = new SparkConf().setAppName("STSCSparkJob")
        val sc = new SparkContext(conf)

        STSC.sparkCluster(sc, args(0), args(1), "clusters")
        STSC.sparkCluster(sc, args(0), args(1), "clusterCenters.csv", true)
        //new File("clusters").delete()
        sc.stop()
    }
}
