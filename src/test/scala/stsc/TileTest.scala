import stsc.Tile
import org.scalatest._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

class TileTest extends FlatSpec with Matchers {
    "The tile" should "work" in {
        val tile = Tile(DenseVector(-1), DenseVector(10), 2)
        tile.sizes() should be(DenseVector(11.0))
        tile.asTranspose() should be(DenseVector(-1.0, 10.0).t)
        val obsOne = DenseVector(5.0)
        val obsTwo = DenseVector(12.0)
        val obsThree = DenseVector(-5.0)
        tile.has(obsOne) should be (true)
        tile.hasDeeply(obsOne) should be (true)
        tile.has(obsTwo) should be (true)
        tile.hasDeeply(obsTwo) should be (false)
        tile.has(obsThree) should be (false)
        tile.hasDeeply(obsThree) should be (false)
    }
}
