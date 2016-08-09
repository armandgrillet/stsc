package stsc

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import org.scalatest.{FlatSpec, Matchers}

class ParallelClusterTest extends FlatSpec with Matchers {
    "The simplest test" should "work" in {
        val conf = new SparkConf().setAppName("ParallelClusterTest").setMaster("local")
        val sc = new SparkContext(conf)
        val a = STSC.parallelCluster(sc)
        println(a)
    }
}
