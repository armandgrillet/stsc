package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class KDTreeTest extends FlatSpec with Matchers {
    "The k-d tree" should "work with tt0" in {
        val dataPath = getClass.getResource("/datasetforkdt0.csv").getPath()
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
        val dataPath = getClass.getResource("/datasetforkdt0.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val tree = KDTree.createWithMaxObservations(matrix, 110, 0, KDTree.cutUsingContentDistances)
        tree.toCSV("ttfortt0.csv")
        val loadedTree = KDTree.fromCSV(getClass.getResource("/kdt0.csv").getPath())
        tree.dimensions should be (loadedTree.dimensions)
        tree.tiles should be (loadedTree.tiles)
        new File("ttfortt0.csv").delete()
    }

    "The k-d tree" should "work with a very simple dataset when clustering with max observations" in {
        val matrix = DenseMatrix((1.0, 3.0), (2.0, 3.0), (3.0, 3.0), (4.0, 3.0), (5.0, 3.0), (3.0, 1.0), (4.0, 1.0), (5.0, 1.0), (6.0, 1.0), (7.0, 1.0))
        val tree = KDTree.createWithMaxObservations(matrix, 5, 0, KDTree.cutUsingTileDimensions)
        // println(tree.tiles.left.value)
        // println(tree.tiles.right.value)
    }

    "The k-d tree" should "work with a large dataset (36 clusters of 100 obs)" in {
        val dataPath = getClass.getResource("/dataset.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val t0 = System.nanoTime()
        val tree = KDTree.createWithLeafsNumber(matrix, 8, 0, KDTree.cutUsingTileDimensions)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        // println(tree.toString())
    }

    "The k-d tree" should "work with a large dataset (100 clusters of 100 obs)" in {
        val dataPath = getClass.getResource("/dataset100.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val t0 = System.nanoTime()
        val tree = KDTree.createWithMaxObservations(matrix, 1000, 0, KDTree.cutUsingTileDimensions)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
    }

    "The k-d tree" should "work with a large dataset (1000 clusters of 100 obs)" in {
        val dataPath = getClass.getResource("/dataset1000.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val t0 = System.nanoTime()
        val tree = KDTree.createWithMaxObservations(matrix, 1000, 0, KDTree.cutUsingTileDimensions)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        tree.toCSV("./kdt1000.csv")
    }

    "The k-d tree" should "work with a large dataset (10000 clusters of 100 obs)" in {
        val dataPath = getClass.getResource("/dataset10000.csv").getPath()
        val dataset = new File(dataPath)
        val matrix = breeze.linalg.csvread(dataset)
        val t0 = System.nanoTime()
        val tree = KDTree.createWithMaxObservations(matrix, 1000, 0, KDTree.cutUsingTileDimensions)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
    }
}
