package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class leafsTest extends FlatSpec with Matchers {
    "Getting leafs" should "work in one dimension" in {
        val tree = new KDTree(Node(
            Tile(DenseVector(0.0), DenseVector(10.0)),
            Node(Tile(DenseVector(0.0), DenseVector(5.0))),
            Node(Tile(DenseVector(5.0), DenseVector(10.0)))
        ), 0.0) // border width)

        tree.leafs should be (Array(Tile(DenseVector(0.0), DenseVector(5.0)), Tile(DenseVector(5.0), DenseVector(10.0))))
    }

    "Getting leafs" should "work in two dimensions" in {
        val tree = new KDTree(Node(
            Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
            Node(Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 5.0))),
            Node(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0)))
        ), 0.0) // border width)

        tree.leafs should be (Array(Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 5.0)), Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0))))
    }
}
