// Just to test.

package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

object Main {
    def main(args: Array[String]) = {
        val tree = TessellationTree.createWithMaxObservations(DenseMatrix((1.0, 3.0), (3.0, 3.0)), 4)
    }
}
