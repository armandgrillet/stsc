package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import org.scalatest.{FlatSpec, Matchers}

class NodeTest extends FlatSpec with Matchers {
    "The tile tree" should "require the value and children's values to be in the same dimensions" in {
        val e = intercept[IllegalArgumentException] {
            val tree = Node(
                Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
                Node(Tile(DenseVector(0.0), DenseVector(5.0))),
                Node(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0)))
            )
        }
        e.getMessage should equal ("requirement failed: The value of the tree and its left child must be in the same dimensions")

        val e2 = intercept[IllegalArgumentException] {
            val tree = Node(
                Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
                Node(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 5.0))),
                Node(Tile(DenseVector(5.0), DenseVector(10.0)))
            )
        }
        e2.getMessage should equal ("requirement failed: The value of the tree and its right child must be in the same dimensions")

        val e3 = intercept[IllegalArgumentException] {
            val tree = Node(
                Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
                Node(Tile(DenseVector(0.0), DenseVector(5.0, 10.0))),
                Node(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0)))
            )
        }
        e3.getMessage should equal ("requirement failed: mins and maxs of the tile have to be the same")
    }
}
