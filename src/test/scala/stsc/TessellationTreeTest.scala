import stsc._
import org.scalatest._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import java.io.File

class TessellationTreeTest extends FlatSpec with Matchers {
    "The tessellation tree" should "work with tt0" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = TessellationTree.createWithMaxObservations(matrix, 110, 0, TessellationTree.cutUsingContentDimensions)
        tree.dimensions should be (2)
        tree.tiles.length should be (4)

        val tree2 = TessellationTree.createWithMaxObservations(matrix, 115, 0)
        tree2.dimensions should be (2)
        tree2.tiles.length should be (4)
    }

    "The tessellation tree" should "be saved and loaded" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = TessellationTree.createWithMaxObservations(matrix, 110, 0, TessellationTree.cutUsingContentDimensions)
        tree.toCSV("ttfortt0.csv")
        val loadedTree = TessellationTree.fromCSV("ttfortt0.csv")
        tree.dimensions should be (loadedTree.dimensions)
        tree.tiles should be (loadedTree.tiles)
        new File("ttfortt0.csv").delete()
    }
}
