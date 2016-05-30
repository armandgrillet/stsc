package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

package object stsc {
    // Anonymous functions to define if the cut should be vertical or horizontal, 0/false = vertical and 1/true = horizontal
    val getDirectionDependingOnTileDimensions = (parentTile: Tile, observations: DenseMatrix[Double]) => {
        val (width, height) = parentTile.dimensions()
        if (width < height) {
            true
        } else {
            false
        }
    }: Boolean

    val getDirectionDependingOnContentDimensions = (parentTile: Tile, observations: DenseMatrix[Double]) => {
        val (width, height) = parentTile.dimensions()
        val minObservationX = min(observations(::, 0))
        val maxObservationX = max(observations(::, 0))
        val minObservationY = min(observations(::, 1))
        val maxObservationY = max(observations(::, 1))
        if (stsc.dist(minObservationX, maxObservationX) < stsc.dist(minObservationY, maxObservationY)) {
            true
        } else {
            false
        }
    }: Boolean

    def dist(a: Double, b: Double): Double = {
        return sqrt(pow(a - b, 2))
    }
}
