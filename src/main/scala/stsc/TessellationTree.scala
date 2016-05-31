package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import java.io.File
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

class TessellationTree(val dimension: Int, val tiles: List[Tile]) {
    def toCSV(filePath: String) = {
        var tilesDenseMatrix = DenseMatrix.zeros[Double](this.tiles.length + 1, dimension * 2)
        tilesDenseMatrix(0, 0) = this.tiles(0).radius // The radius
        var i = 0
        for (i <- 0 until tiles.length) {
            tilesDenseMatrix(i + 1, ::) := tiles(i).transpose()
        }
        csvwrite(new File(filePath), tilesDenseMatrix, separator = ',')
    }

    // Return the tiles owning an object.
    def owningTiles(observation: DenseVector[Double]): List[Tile] = {
        var owningTiles = ListBuffer.empty[Tile]
        for (tile <- this.tiles) {
            if (tile.has(observation)) {
                owningTiles += tile
                if (owningTiles.length == 1 && tile.hasDeeply(observation)) {
                    return owningTiles.toList
                }
            }
        }
        return owningTiles.toList
    }
}

object TessellationTree {
    // Initialization functions.
    def createWithMaxObservations(dataset: DenseMatrix[Double], maxObservations: Int, tileRadius: Double, cutDirectionFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile) = stsc.cutUsingTileDimensions): TessellationTree = {
        if (tileRadius < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileRadius + ".") }
        if (maxObservations > dataset.rows) { throw new IndexOutOfBoundsException("The maximum number of observations in a tile must be less than the dataset rows.") }
        val firstTile = Tile(DenseVector.fill(dataset.cols){scala.Double.NegativeInfinity}, DenseVector.fill(dataset.cols){scala.Double.PositiveInfinity}, tileRadius)
        val tiles: List[Tile] = cutWithMaxObservations(dataset, firstTile, maxObservations, cutDirectionFunction)
        return new TessellationTree(dataset.cols, tiles)
    }

    /*def createWithTilesNumber(dataset: DenseMatrix[Double], tileRadius: Double = 0, tilesNumber: Int): TessellationTree = {

}*/

def fromCSV(filePath: String): TessellationTree = {
    var tilesDenseMatrix = csvread(new File(filePath))
    if (tilesDenseMatrix.cols % 2 != 0) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree.") }
    if (tilesDenseMatrix(0, 0) != sum(tilesDenseMatrix(0, ::))) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree.") }

    val radius = tilesDenseMatrix(0, 0)
    val dimension = tilesDenseMatrix.cols / 2
    val tiles = ListBuffer.empty[Tile]
    var i = 0
    for (i <- 1 until tilesDenseMatrix.rows) {
        val tile = tilesDenseMatrix(::, i)
        tiles += Tile(tile(0 until dimension), tile(dimension to -1), radius)
    }
    return new TessellationTree(dimension, tiles.toList)
}

private def observationsInTile(dataset: DenseMatrix[Double], tile: Tile, axis: Int): DenseMatrix[Double] = {
    var observations = DenseMatrix.zeros[Double](dataset.rows, dataset.cols)
    var numberOfObservations = 0
    for (row <- dataset(*,::)) {
        if (tile.has(row)) {
            observations(numberOfObservations, 0) = row(0)
            observations(numberOfObservations, 1) = row(1)
            numberOfObservations += 1
        }
    }
    return observations(::, 0 until numberOfObservations)
}

private def cutWithMaxObservations(dataset: DenseMatrix[Double], parentTile: Tile, maxObservations: Int, cutDirectionFunction: (Tile, DenseMatrix[Double]) => (Tile, Tile)): List[Tile] = {
    val observations = observationsInTile(dataset, parentTile, 0)
    if (observations.rows > maxObservations) {
        val childrenTiles = cutDirectionFunction(parentTile, observations)
        return List.concat(cutWithMaxObservations(observations, childrenTiles._1, maxObservations, cutDirectionFunction), cutWithMaxObservations(observations, childrenTiles._2, maxObservations, cutDirectionFunction))
    } else {
        return List(parentTile)
    }
}
}
