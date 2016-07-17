package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class LeafsAsDenseMatrixTest extends FlatSpec with Matchers {
    "Getting leafs as dense matrix " should "work in one dimension" in {
        val tree = new KDTree(Node(
            Tile(DenseVector(0.0), DenseVector(10.0)),
            Node(Tile(DenseVector(0.0), DenseVector(5.0))),
            Node(Tile(DenseVector(5.0), DenseVector(10.0)))
        ), 0.0) // border width)
        val leafs = tree.leafsAsDenseMatrix

        leafs should be (DenseMatrix((0.0, 5.0), (5.0, 10.0)))
    }

    "Getting leafs as dense matrix " should "work in two dimensions" in {
        val tree = new KDTree(Node(
            Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
            Node(Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 5.0))),
            Node(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0)))
        ), 0.0) // border width)
        val leafs = tree.leafsAsDenseMatrix

        leafs should be (DenseMatrix((0.0, 0.0, 10.0, 5.0), (0.0, 5.0, 10.0, 10.0)))
    }
}
