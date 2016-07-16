package stsc

import breeze.linalg.{DenseMatrix, DenseVector}

import org.scalatest.{FlatSpec, Matchers}

class TileTreeTest extends FlatSpec with Matchers {
    "The tile tree" should "require the value and children's values to be in the same dimensions" in {
        val e = intercept[IllegalArgumentException] {
            val tree = TileTree(
                Tile(DenseVector(0.0, 0.0), DenseVector(10.0, 10.0)),
                TileTree(Tile(DenseVector(0.0), DenseVector(5.0))),
                TileTree(Tile(DenseVector(0.0, 5.0), DenseVector(10.0, 10.0)))
            )
        }
        e.getMessage should equal ("The value of the tree and its left child must be in the same dimensions")
    }
}
