package stsc

import breeze.linalg.{BitVector, DenseMatrix, DenseVector, argmax, csvread, csvwrite, max, min, sum, *}
import breeze.numerics.abs
import breeze.stats.median

import java.io.File
import scala.collection.mutable.ListBuffer

/** A k-d tree to divide a daset into multiple tiles.
*
* @constructor create a new k-d tree containing a list of Tiles and a dimension.
* @param tiles the tiles in the k-d tree, represented as a binary tree
* @param borderWidth the bordel of each tile that will be used to know where is an observation
*/
class KDTree(val tiles: Node, val borderWidth: Double) {
    require(borderWidth >= 0)

    val dimensions = tiles.value.mins.length

    def leafs(): List[Tile] = {
        val leafs = List.fill(tiles.leafs)(Tile(DenseVector.zeros[Double](1), DenseVector.zeros[Double](1)))
        var count = 0
        def leafsHelper(tree: Node) {
            if (tree.isLeaf) {
                leafs.updated(count, tree.value)
                count += 1
            } else {
                leafsHelper(tree.left)
                leafsHelper(tree.right)
            }
        }
        leafsHelper(tiles)

        return leafs
    }

    /* Returns a densematrix containing all the leafs of the tree. */
    def leafsAsDenseMatrix(): DenseMatrix[Double] = {
        val leafsDenseMatrix = DenseMatrix.zeros[Double](tiles.leafs, dimensions * 2)
        var count = 0
        def asDenseMatrixHelper(tree: Node) {
            if (tree.isLeaf) {
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

    /** Write the k-d tree as a CSV file.
    *
    * @param filePath the path where the k-d tree has to be written.
    */
    def toCSV(filePath: String) = {
        var tilesDenseMatrix = DenseMatrix.zeros[Double](tiles.length + 1, dimensions * 2)
        tilesDenseMatrix(0, 0) = borderWidth

        val tilesDenseVector = DenseVector.fill(tiles.length){ Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0)) }

        def toCSVHelper(tree: Node, position: Int) {
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

        def owningTilesHelper(Node: Node) {
            if (Node.isLeaf) {
                owningTiles += Node.value
            } else {
                // The observation can be in multiple tiles thus we test for the left and the right tile.
                if (Node.left.value.has(observation, borderWidth)) {
                    owningTilesHelper(Node.left)
                }
                if (Node.right.value.has(observation, borderWidth)) {
                    owningTilesHelper(Node.right)
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
        def owningTileHelper(Node: Node): Tile = {
            if (Node.isLeaf) {
                return Node.value
            } else {
                if (Node.left.value.has(observation, 0)) {
                    return owningTileHelper(Node.left)
                } else {
                    return owningTileHelper(Node.right)
                }
            }
        }

        return owningTileHelper(tiles)
    }
}

/** Factory for gr.armand.stsc.KDTree instances. */
object KDTree {
    /** Initialize a k-d tree with a given maximum number of observations per tile.
    *
    * @param dataset the dataset to use to create the k-d tree.
    * @param maxObservations the maximum number of observations per tile.
    * @param tileBorderWidth the border width of the tiles in the k-d tree.
    * @param cutFunction the function used to cut a tile in two. The default is to cut the tiles using the parent tile dimensions.
    * @return the k-d tree.
    */
    def createWithMaxObservations(dataset: DenseMatrix[Double], maxObservations: Int, tileBorderWidth: Double = 0, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile) = cutUsingTileDimensions): KDTree = {
        if (tileBorderWidth < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileBorderWidth + ".") }
        if (maxObservations > dataset.rows) { throw new IndexOutOfBoundsException("The maximum number of observations in a tile must be less than the number of observations.") }
        val firstTile = Tile(DenseVector.fill(dataset.cols){scala.Double.NegativeInfinity}, DenseVector.fill(dataset.cols){scala.Double.PositiveInfinity})
        val tilesTree = cutWithMaxObservations(dataset, firstTile, maxObservations, cutFunction)
        return new KDTree(tilesTree, tileBorderWidth)
    }

    /** Initialize a k-d tree with a given maximum number of tiles in the k-d tree.
    *
    * @param dataset the dataset to use to create the k-d tree.
    * @param nodesNumber the number of nodes in the k-d tree.
    * @param tileBorderWidth the border width of the tiles in the k-d tree.
    * @param cutFunction the function used to cut a tile in two. The default is to cut the tiles using the parent tile dimensions.
    * @return the k-d tree.
    */
    def createWithTilesNumber(dataset: DenseMatrix[Double], nodesNumber: Int, tileBorderWidth: Double = 0, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile) = cutUsingTileDimensions): KDTree = {
        if (tileBorderWidth < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileBorderWidth + ".") }
        if (nodesNumber < 1) { throw new IndexOutOfBoundsException("The number of tiles must be positive.") }
        if (nodesNumber > dataset.rows) { throw new IndexOutOfBoundsException("The number of tiles must be less than the number of observations.") }
        val maxObservations = math.ceil(dataset.rows / nodesNumber).toInt
        val firstTile = Tile(DenseVector.fill(dataset.cols){scala.Double.NegativeInfinity}, DenseVector.fill(dataset.cols){scala.Double.PositiveInfinity})
        val tilesTree = cutWithMaxObservations(dataset, firstTile, maxObservations, cutFunction)
        return new KDTree(tilesTree, tileBorderWidth)
    }

    /** Initialize a k-d tree using a given CSV. The CSV must have a specific structure created by toCSV().
    *
    * @param filePath the path of the CSV file containing the k-d tree.
    * @return the k-d tree.
    */
    def fromCSV(filePath: String): KDTree = {
        var tilesDenseMatrix = csvread(new File(filePath))
        if (tilesDenseMatrix.cols % 2 != 0) { throw new IndexOutOfBoundsException("The file is not formatted to be a k-d tree.") }
        if (tilesDenseMatrix(0, 0) != sum(tilesDenseMatrix(0, ::))) { throw new IndexOutOfBoundsException("The file is not formatted to be a k-d tree.") }

        val borderWidth = tilesDenseMatrix(0, 0)
        val dimensions = tilesDenseMatrix.cols / 2
        val tilesDenseVector = DenseVector.fill(tilesDenseMatrix.rows - 1){ Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0)) }
        var i = 0
        for (i <- 1 until tilesDenseMatrix.rows) {
            val rowAsTile = tilesDenseMatrix(i, ::).t
            tilesDenseVector(i - 1) = Tile(rowAsTile(0 until dimensions), rowAsTile(dimensions to -1))
        }

        def fromCSVHelper(i: Int): Node = {
            if (2 * i + 2 < tilesDenseVector.length) {
                return Node(tilesDenseVector(i), fromCSVHelper(2 * i + 1), fromCSVHelper(2 * i + 2))
            } else {
                return Node(tilesDenseVector(i))
            }
        }

        var tiles = fromCSVHelper(0)

        return new KDTree(tiles, borderWidth)
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
        val parentSizes = parent.sizes()
        // We chekc if the parent sites contains some infinities.
        if ((parentSizes :== DenseVector.fill(parentSizes.length){Double.PositiveInfinity}) == BitVector()) {
            val cutDirection = argmax(parentSizes)
            println(parent.sizes())
            println(cutDirection)
            val observationsMedian = median(observations(::, cutDirection))

            var firstTileMaxs = parent.maxs.copy
            firstTileMaxs(cutDirection) = observationsMedian
            var firstTile = new Tile(parent.mins, firstTileMaxs) // The children parents will be similar as the parent.

            var secondTileMins = parent.mins.copy
            secondTileMins(cutDirection) = observationsMedian
            var secondTile = Tile(secondTileMins, parent.maxs)

            (firstTile, secondTile)
        } else {
            cutUsingContentDistances(parent, observations)
        }
    }: (Tile, Tile)

    /** Cut a tile in two depending on the observations in a tile, returns two new tiles.
    *
    * @param tile the parent tile.
    * @param observations the observations in the tile.
    */
    val cutUsingContentDistances = (parent: Tile, observations: DenseMatrix[Double]) => {
        val minCols = min(observations(::, *)).t
        val maxCols = max(observations(::, *)).t
        val dists = abs(maxCols - minCols)
        val cutDirection = argmax(dists)
        val observationsMedian = median(observations(::, cutDirection))

        var firstTileMaxs = parent.maxs.copy
        firstTileMaxs(cutDirection) = observationsMedian
        var firstTile = new Tile(parent.mins, firstTileMaxs) // The children parents will be similar as the parent.

        var secondTileMins = parent.mins.copy
        secondTileMins(cutDirection) = observationsMedian
        var secondTile = Tile(secondTileMins, parent.maxs)

        (firstTile, secondTile)
    }: (Tile, Tile)

    private[stsc] def cutWithMaxObservations(dataset: DenseMatrix[Double], parentTile: Tile, maxObservations: Int, cutFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile)): Node = {
        val observations = parentTile.filter(dataset, 0)
        if (observations.rows > maxObservations) {
            val childrenTiles = cutFunction(parentTile, observations)
            return Node(parentTile, cutWithMaxObservations(observations, childrenTiles._1, maxObservations, cutFunction), cutWithMaxObservations(observations, childrenTiles._2, maxObservations, cutFunction))
        } else {
            return Node(parentTile)
        }
    }
}
