import stsc._
import org.scalatest._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

class VertStackTest extends FlatSpec with Matchers {
    "The vertical stack " should "work with a 2x2 matrix" in {
        val matrix = DenseMatrix((1.0, 3.0), (3.0, 3.0))
        val verticalStack = vertStack(matrix, 2)
        val correctStack = DenseMatrix((1.0, 3.0), (3.0, 3.0), (1.0, 3.0), (3.0, 3.0))
        verticalStack should be (correctStack)
    }

    "The vertical stack " should "work with a 3x3 matrix" in {
        val matrix = DenseMatrix((1.0, 3.0), (2.0, 3.0), (3.0, 3.0))
        val verticalStack = vertStack(matrix, 3)
        val correctStack = DenseMatrix((1.0, 3.0), (2.0, 3.0), (3.0, 3.0), (1.0, 3.0), (2.0, 3.0), (3.0, 3.0), (1.0, 3.0), (2.0, 3.0), (3.0, 3.0))
        verticalStack should be (correctStack)
    }
}
