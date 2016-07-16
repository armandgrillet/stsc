package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class TessellationTreeTest extends FlatSpec with Matchers {
    "The tessellation tree" should "work with tt0" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = TessellationTree.createWithMaxObservations(matrix, 110, 0, TessellationTree.cutUsingContentDistances)
        tree.dimensions should be (2)
        tree.tiles.leafs should be (4)
        tree.tiles.length should be (7)

        val tree2 = TessellationTree.createWithMaxObservations(matrix, 115, 0)
        tree2.dimensions should be (2)
        tree2.tiles.leafs should be (4)
        tree2.tiles.length should be (7)
    }

    "The tessellation tree" should "be saved and loaded" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = TessellationTree.createWithMaxObservations(matrix, 110, 0, TessellationTree.cutUsingContentDistances)
        tree.toCSV("ttfortt0.csv")
        val loadedTree = TessellationTree.fromCSV("ttfortt0.csv")
        tree.dimensions should be (loadedTree.dimensions)
        tree.tiles should be (loadedTree.tiles)
        new File("ttfortt0.csv").delete()
    }

    "The tessellation tree" should "work with a simple dataset" in {
        val matrix = DenseMatrix((0.0, 0.0), (0.0, 4.0), (6.0, 0.0), (6.0, 4.0), (14.0, 8.0), (16.0, 8.0), (14.0, 15.0), (16.0, 15.0))
        val tree = TessellationTree.createWithMaxObservations(matrix, 2, 0, TessellationTree.cutUsingTileDimensions)
        println(tree.tiles.left.value)
        println(tree.tiles.right.value)
        println(tree.tiles.left.left.value)
        println(tree.tiles.left.right.value)
        println(tree.tiles.right.left.value)
        println(tree.tiles.right.right.value)
    }
}
