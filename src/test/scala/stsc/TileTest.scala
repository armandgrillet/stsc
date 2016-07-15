package stsc

import breeze.linalg.DenseVector

import org.scalatest.{FlatSpec, Matchers}

class TileTest extends FlatSpec with Matchers {
    "The tile" should "work" in {
        val tessellationTreeBorderWidth = 2
        val tile = Tile(DenseVector(-1), DenseVector(10))
        tile.sizes() should be(DenseVector(11.0))
        tile.asTranspose() should be(DenseVector(-1.0, 10.0).t)

        val obsOne = DenseVector(5.0)
        tile.has(obsOne, tessellationTreeBorderWidth) should be (true)
        tile.hasDeeply(obsOne, tessellationTreeBorderWidth) should be (true)

        val obsTwo = DenseVector(12.0)
        tile.has(obsTwo, tessellationTreeBorderWidth) should be (true)
        tile.hasDeeply(obsTwo, tessellationTreeBorderWidth) should be (false)

        val obsThree = DenseVector(-5.0)
        tile.has(obsThree, tessellationTreeBorderWidth) should be (false)
        tile.hasDeeply(obsThree, tessellationTreeBorderWidth) should be (false)

        val obsFour = DenseVector(-1.0)
        tile.has(obsFour, 0.0) should be (true)

        val obsFive = DenseVector(10.0)
        tile.has(obsFive, 0.0) should be (false)
    }
}
