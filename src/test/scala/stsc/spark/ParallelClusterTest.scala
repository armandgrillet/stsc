package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import org.scalatest.{FlatSpec, Matchers}

class ParallelClusterTest extends FlatSpec with Matchers {
    "The simplest test" should "work" in {
        // val conf = new SparkConf().setAppName("ParallelClusterTest").setMaster("local")
        // val sc = new SparkContext(conf)

        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val ttPath = getClass.getResource("/kdt0.csv").getPath()
        val a = STSC.sparkCluster(dataPath, ttPath)
        println(a)
    }
}
