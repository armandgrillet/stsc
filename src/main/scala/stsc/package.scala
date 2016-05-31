package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

package object stsc {
    // Anonymous functions to define if the cut should be vertical or horizontal, 0 = vertical (X) and 1 = horizontal (Y) and 2 = Z
    val cutUsingTileDimensions = (tile: Tile, observations: DenseMatrix[Double]) => {
        val cutDirection = argmax(tile.dimensions())
        val median = breeze.stats.median(observations(::, cutDirection))
        val firstTile, secondTile = tile
        firstTile.maxs(cutDirection) = median
        secondTile.mins(cutDirection) = median
        (firstTile, secondTile)
    }: (Tile, Tile)

    val cutUsingContentDimensions = (tile: Tile, observations: DenseMatrix[Double]) => {
        val minCols = min(observations(::, *)).t
        val maxCols = max(observations(::, *)).t
        val dists = abs(maxCols - minCols)
        val cutDirection = argmax(dists)

        val median = breeze.stats.median(observations(::, cutDirection))
        val firstTile, secondTile = tile
        firstTile.maxs(cutDirection) = median
        secondTile.mins(cutDirection) = median
        (firstTile, secondTile)
    }: (Tile, Tile)
}
