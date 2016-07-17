package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class KDTreeTest extends FlatSpec with Matchers {
    "The k-d tree" should "work with tt0" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = KDTree.createWithMaxObservations(matrix, 110, 0, KDTree.cutUsingContentDistances)
        tree.dimensions should be (2)
        tree.tiles.leafs should be (4)
        tree.tiles.length should be (7)

        val tree2 = KDTree.createWithMaxObservations(matrix, 115, 0)
        tree2.dimensions should be (2)
        tree2.tiles.leafs should be (4)
        tree2.tiles.length should be (7)
    }

    "The k-d tree" should "be saved and loaded" in {
        val dataPath = getClass.getResource("/tt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = KDTree.createWithMaxObservations(matrix, 110, 0, KDTree.cutUsingContentDistances)
        tree.toCSV("ttfortt0.csv")
        val loadedTree = KDTree.fromCSV("ttfortt0.csv")
        tree.dimensions should be (loadedTree.dimensions)
        tree.tiles should be (loadedTree.tiles)
        new File("ttfortt0.csv").delete()
    }

    "The k-d tree" should "work with a very simple dataset" in {
        val matrix = DenseMatrix((1.0, 3.0), (2.0, 3.0), (3.0, 3.0), (4.0, 3.0), (5.0, 3.0), (3.0, 1.0), (4.0, 1.0), (5.0, 1.0), (6.0, 1.0), (7.0, 1.0))
        val tree = KDTree.createWithMaxObservations(matrix, 5, 0, KDTree.cutUsingTileDimensions)
        println(tree.tiles.left.value)
        println(tree.tiles.right.value)
    }

    "The k-d tree" should "work with a simple dataset" in {
        val matrix = DenseMatrix((0.0, 0.0), (0.0, 4.0), (6.0, 0.0), (6.0, 4.0), (14.0, 8.0), (16.0, 8.0), (14.0, 15.0), (16.0, 15.0))
        val tree = KDTree.createWithMaxObservations(matrix, 2, 0, KDTree.cutUsingTileDimensions)
    }
}
