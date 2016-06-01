package stsc

/** Provides a tesselation tree class to cut a dataset and the self-tuning spectral clustering algorithm.
  *
  * ==Overview==
  * The main class to use is the self-tuning spectral clustering algorithm, [[gr.armand.stsc.Algorithm]]:
  * {{{
  * scala> import gr.armand.stsc.Algorithm
  * scala> val (clustersQualities, correctClusters) = Algorithm.cluster(dataset)
  * }}}
  *
  * If you include [[gr.armand.stsc.TessellationTree]], you can
  * also create a tessellation tree to divide a dataset:
  * {{{
  * scala> import gr.armand.stsc.TessellationTree
  * scala> val tree = TessellationTree.createWithMaxObservations(dataset, maxObservationsPerTile, tileBorderWidth)
  * }}}
  *
  * The third class of the library is Tile, a list of tiles compose a tessellation tree.
  * A Tile is composed of two DenseVectors representing the minimums and maximums in every dimensions.
  * For example, if a tessellation tree is in two dimensions and
  */

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

/** This is the Scaladoc for the package. */
package object stsc {
    // Anonymous functions to find on which dimension should the cut of the tile be made.
    /** Cut a tile in two depending on its dimensions, returns two new tiles.
      *
      * If the tile is in two dimensions and the distance between maxs(0) and mins(0) is bigger than
      * maxs(1) and mins(1) the cut direction will be 0 as we need to cut in the first dimension (0).
      *
      * @param tile the parent tile.
      * @param observations the observations in the tile.
      */
    val cutUsingTileDimensions = (tile: Tile, observations: DenseMatrix[Double]) => {
        val cutDirection = argmax(tile.dimensions())
        val median = breeze.stats.median(observations(::, cutDirection))
        val firstTile, secondTile = tile // The children tiles will be similar as the parent.
        firstTile.maxs(cutDirection) = median
        secondTile.mins(cutDirection) = median + Double.MinPositiveValue // We do not want the two tiles to overlap.
        (firstTile, secondTile)
    }: (Tile, Tile)

    /** Cut a tile in two depending on the observations in a tile, returns two new tiles.
      *
      * @param tile the parent tile.
      * @param observations the observations in the tile.
      */
    val cutUsingContentDimensions = (tile: Tile, observations: DenseMatrix[Double]) => {
        val minCols = min(observations(::, *)).t
        val maxCols = max(observations(::, *)).t
        val dists = abs(maxCols - minCols)
        val cutDirection = argmax(dists)
        val median = breeze.stats.median(observations(::, cutDirection))
        val firstTile, secondTile = tile
        firstTile.maxs(cutDirection) = median
        secondTile.mins(cutDirection) = median + Double.MinPositiveValue
        (firstTile, secondTile)
    }: (Tile, Tile)
}
