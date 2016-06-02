// Just to test.

package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import java.io.File

object Main {
    def main(args: Array[String]) = {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = TessellationTree.createWithMaxObservations(matrix, 115, 0, stsc.cutUsingContentDimensions)
        println(tree.tiles)
    }
}
