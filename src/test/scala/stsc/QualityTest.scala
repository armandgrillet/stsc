import stsc._
import org.scalatest._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

class QualityTest extends FlatSpec with Matchers {
    "The quality " should "work with a perfect 2x2 matrix" in {
        val matrix = DenseMatrix((1.0, 0.0), (0.0, 1.0))
        val computedQuality = evaluateQuality(matrix)
        val correctQuality = 1.0
        computedQuality should be (correctQuality)
    }
}
