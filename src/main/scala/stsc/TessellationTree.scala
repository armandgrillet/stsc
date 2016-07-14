package stsc

import breeze.linalg.{DenseMatrix, DenseVector, argmax, csvread, csvwrite, max, min, sum, *}
import breeze.numerics.abs
import breeze.stats.median

import java.io.File
import scala.collection.mutable.ListBuffer

/** A tessellation tree to divide a daset into multiple tiles.
*
* @constructor create a new tessellation tree containing a list of Tiles and a dimension.
* @param dimensions the dimension which in is the tessellation tree
* @param tiles the tiles in the tessellation tree, represented as a binary tree
*/
class TessellationTree(val tiles: TileTree) {
    val dimensions = tiles.value.mins.length
    
    /* Returns a densematrix containing all the leafs of the tree. */
    def leafsAsDenseMatrix(): DenseMatrix[Double] = {
        val leafsDenseMatrix = DenseMatrix.zeros[Double](tiles.leafs, dimensions * 2)
        var count = 0
        def asDenseMatrixHelper(tree: TileTree) {
            if (tree.isLeaf) {
                println(tree.value.asTranspose())
                leafsDenseMatrix(count, ::) := tree.value.asTranspose()
                count += 1
            } else {
                asDenseMatrixHelper(tree.left)
                asDenseMatrixHelper(tree.right)
            }
        }
        asDenseMatrixHelper(tiles)

        return leafsDenseMatrix
    }

    /** Write the tessellation tree as a CSV file.
    *
    * @param filePath the path where the tessellation tree has to be written.
    */
    def toCSV(filePath: String) = {
        var tilesDenseMatrix = DenseMatrix.zeros[Double](tiles.length + 1, dimensions * 2)
        tilesDenseMatrix(0, 0) = tiles.value.borderWidth

        val tilesDenseVector = DenseVector.fill(tiles.length){ Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0), 0) }

        def toCSVHelper(tree: TileTree, position: Int) {
            tilesDenseVector(position) = tree.value
            if (!tree.isLeaf) {
                toCSVHelper(tree.left, 2 * position + 1)
                toCSVHelper(tree.right, 2 * position + 2)
            }
        }
        toCSVHelper(tiles, 0)

        var i = 0
        for (i <- 0 until tiles.length) {
            tilesDenseMatrix(i + 1, ::) := tilesDenseVector(i).asTranspose()
        }

        csvwrite(new File(filePath), tilesDenseMatrix, separator = ',')
    }

    /** Returns the tile(s) owning a given observation.
    *
    * @param observation the observation to check, represented as a DenseVector where every value is the coordinates in a dimension.
    * @return a list of the tiles having the observation. There can be more than one tile due to the border width.
    */
    def owningTiles(observation: DenseVector[Double]): List[Tile] = {
        var owningTiles = ListBuffer.empty[Tile]

        def owningTilesHelper(tileTree: TileTree) {
            if (tileTree.isLeaf) {
                owningTiles += tileTree.value
            } else {
                // The observation can be in multiple tiles thus we test for the left and the right tile.
                if (tileTree.left.value.has(observation)) {
                    owningTilesHelper(tileTree.left)
                }
                if (tileTree.right.value.has(observation)) {
                    owningTilesHelper(tileTree.right)
                }
            }
        }

        owningTilesHelper(tiles)
        return owningTiles.toList
    }

    /** Returns the tile owning a given observation, must be within the strict edges of the tile.
    *
    * @param observation the observation to check, represented as a DenseVector where every value is the coordinates in a dimension.
    * @return a list of the tiles having the observation. There can be more than one tile due to the border width.
    */
    def owningTile(observation: DenseVector[Double]): Tile = {
        def owningTileHelper(tileTree: TileTree): Tile = {
            if (tileTree.isLeaf) {
                return tileTree.value
            } else {
                if (tileTree.left.value.has(observation, 0)) {
                    return owningTileHelper(tileTree.left)
                } else {
                    return owningTileHelper(tileTree.right)
                }
            }
        }

        return owningTileHelper(tiles)
    }
}

/** Factory for gr.armand.stsc.TessellationTree instances. */
object TessellationTree {
    /** Initialize a tessellation tree with a given maximum number of observations per tile.
    *
    * @param dataset the dataset to use to create the tessellation tree.
    * @param maxObservations the maximum number of observations per tile.
    * @param tileBorderWidth the border width of the tiles in the tessellation tree.
    * @param cutFunction the function used to cut a tile in two. The default is to cut the tiles using the parent tile dimensions.
    * @return the tessellation tree.
    */
    def createWithMaxObservations(dataset: DenseMatrix[Double], maxObservations: Int, tileBorderWidth: Double, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile) = cutUsingTileDimensions): TessellationTree = {
        if (tileBorderWidth < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileBorderWidth + ".") }
        if (maxObservations > dataset.rows) { throw new IndexOutOfBoundsException("The maximum number of observations in a tile must be less than the number of observations.") }
        val firstTile = Tile(DenseVector.fill(dataset.cols){scala.Double.NegativeInfinity}, DenseVector.fill(dataset.cols){scala.Double.PositiveInfinity}, tileBorderWidth)
        val tilesTree = cutWithMaxObservations(dataset, firstTile, maxObservations, cutFunction)
        return new TessellationTree(tilesTree)
    }

    /** Initialize a tessellation tree with a given maximum number of tiles in the tessellation tree.
    *
    * @param dataset the dataset to use to create the tessellation tree.
    * @param tilesNumber the number of tiles in the tessellation tree.
    * @param tileBorderWidth the border width of the tiles in the tessellation tree.
    * @param cutFunction the function used to cut a tile in two. The default is to cut the tiles using the parent tile dimensions.
    * @return the tessellation tree.
    */
    def createWithTilesNumber(dataset: DenseMatrix[Double], tilesNumber: Int, tileBorderWidth: Double = 0, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile) = cutUsingTileDimensions): TessellationTree = {
        if (tileBorderWidth < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileBorderWidth + ".") }
        if (tilesNumber > dataset.rows) { throw new IndexOutOfBoundsException("The number of tiles must be less than the number of observations.") }
        val maxObservations = math.ceil(dataset.rows / tilesNumber).toInt
        val firstTile = Tile(DenseVector.fill(dataset.cols){scala.Double.NegativeInfinity}, DenseVector.fill(dataset.cols){scala.Double.PositiveInfinity}, tileBorderWidth)
        val tilesTree = cutWithMaxObservations(dataset, firstTile, maxObservations, cutFunction)
        return new TessellationTree(tilesTree)
    }

    /** Initialize a tessellation tree using a given CSV. The CSV must have a specific structure created by toCSV().
    *
    * @param filePath the path of the CSV file containing the tessellation tree.
    * @return the tessellation tree.
    */
    def fromCSV(filePath: String): TessellationTree = {
        var tilesDenseMatrix = csvread(new File(filePath))
        if (tilesDenseMatrix.cols % 2 != 0) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree.") }
        if (tilesDenseMatrix(0, 0) != sum(tilesDenseMatrix(0, ::))) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree.") }

        val borderWidth = tilesDenseMatrix(0, 0)
        val dimensions = tilesDenseMatrix.cols / 2
        val tilesDenseVector = DenseVector.fill(tilesDenseMatrix.rows - 1){ Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0), 0) }
        var i = 0
        for (i <- 1 until tilesDenseMatrix.rows) {
            val rowAsTile = tilesDenseMatrix(i, ::).t
            tilesDenseVector(i - 1) = Tile(rowAsTile(0 until dimensions), rowAsTile(dimensions to -1), borderWidth)
        }

        def fromCSVHelper(i: Int): TileTree = {
            if (2 * i + 2 < tilesDenseVector.length) {
                return TileTree(tilesDenseVector(i), fromCSVHelper(2 * i + 1), fromCSVHelper(2 * i + 2))
            } else {
                return TileTree(tilesDenseVector(i))
            }
        }

        var tiles = fromCSVHelper(0)

        return new TessellationTree(tiles)
    }

    // Anonymous functions to find on which dimension should the cut of the tile be made.
    /** Cut a tile in two depending on its dimensions, returns two new tiles.
    *
    * If the tile is in two dimensions and the distance between maxs(0) and mins(0) is bigger than
    * maxs(1) and mins(1) the cut direction will be 0 as we need to cut in the first dimension (0).
    *
    * @param tile the parent tile.
    * @param observations the observations in the tile.
    */
    val cutUsingTileDimensions = (parent: Tile, observations: DenseMatrix[Double]) => {
        val cutDirection = argmax(parent.sizes())
        val observationsMedian = median(observations(::, cutDirection))

        var firstTileMaxs = parent.maxs.copy
        firstTileMaxs(cutDirection) = observationsMedian
        var firstTile = new Tile(parent.mins, firstTileMaxs, parent.borderWidth) // The children parents will be similar as the parent.

        var secondTileMins = parent.mins.copy
        secondTileMins(cutDirection) = observationsMedian
        var secondTile = Tile(secondTileMins, parent.maxs, parent.borderWidth)

        (firstTile, secondTile)
    }: (Tile, Tile)

    /** Cut a tile in two depending on the observations in a tile, returns two new tiles.
    *
    * @param tile the parent tile.
    * @param observations the observations in the tile.
    */
    val cutUsingContentDimensions = (parent: Tile, observations: DenseMatrix[Double]) => {
        val minCols = min(observations(::, *)).t
        val maxCols = max(observations(::, *)).t
        val dists = abs(maxCols - minCols)
        val cutDirection = argmax(dists)
        val observationsMedian = median(observations(::, cutDirection))

        var firstTileMaxs = parent.maxs.copy
        firstTileMaxs(cutDirection) = observationsMedian
        var firstTile = new Tile(parent.mins, firstTileMaxs, parent.borderWidth) // The children parents will be similar as the parent.

        var secondTileMins = parent.mins.copy
        secondTileMins(cutDirection) = observationsMedian
        var secondTile = Tile(secondTileMins, parent.maxs, parent.borderWidth)

        (firstTile, secondTile)
    }: (Tile, Tile)

    private[stsc] def cutWithMaxObservations(dataset: DenseMatrix[Double], parentTile: Tile, maxObservations: Int, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile)): TileTree = {
        val observations = observationsInTile(dataset, parentTile)
        if (observations.rows > maxObservations) {
            val childrenTiles = cutFunction(parentTile, observations)
            return TileTree(parentTile, cutWithMaxObservations(observations, childrenTiles._1, maxObservations, cutFunction), cutWithMaxObservations(observations, childrenTiles._2, maxObservations, cutFunction))
        } else {
            return TileTree(parentTile)
        }
    }

    private[stsc] def observationsInTile(dataset: DenseMatrix[Double], tile: Tile): DenseMatrix[Double] = {
        val observations = DenseMatrix.zeros[Double](dataset.rows, dataset.cols)
        var numberOfObservations = 0
        for (row <- dataset(*,::)) {
            if (tile.has(row)) {
                observations(numberOfObservations, ::) := row.t
                numberOfObservations += 1
            }
        }
        return observations(0 until numberOfObservations, ::)
    }
}
