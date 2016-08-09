package stsc

import breeze.linalg.DenseMatrix

import java.io.File
import org.scalameter._
import org.scalatest.FunSuite

class STSCSimpleBenchmark extends FunSuite {
    test("Test one of the simplest possible case") {
        val time = measure {
            val dataPath = getClass.getResource("/simplestCase.csv").getPath()
            val dataset = new File(dataPath)
            val matrix = breeze.linalg.csvread(dataset)
            val result = STSC.cluster(matrix, 2, 2)
        }
        println("Total time : " + time)
    }
}
