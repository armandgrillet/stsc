package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import java.io.File
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

class TessellationTree(val tiles: List[Tile], val tileRadius: Double) {
    def toCSV(filePath: String) = {
        var tilesDenseMatrix = DenseMatrix.zeros[Double](this.tiles.length + 1, 4)
        tilesDenseMatrix(0, ::) := DenseVector(this.tileRadius, 0, 0, 0).t // First line = radius.
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
                if (owningTiles.length == 1 && tile.hasDeeply(observation, this.tileRadius)) {
                    return owningTiles.toList
                }
            }
        }
        return owningTiles.toList
    }
}

object TessellationTree {
    // Initialization functions.
    def createWithMaxObservations(dataset: DenseMatrix[Double], maxObservations: Int, tileRadius: Double = 0, cutDirectionFunction: (Tile, DenseMatrix[Double]) => Boolean = stsc.getDirectionDependingOnTileDimensions): TessellationTree = {
        if (tileRadius < 0) { throw new IndexOutOfBoundsException("Tile radius must be a positive number. It was " + tileRadius + ".") }
        if (dataset.cols != 2) { throw new IndexOutOfBoundsException("The tessellation tree only works with two-dimensional datasets.") }
        if (maxObservations > dataset.rows) { throw new IndexOutOfBoundsException("The maximum number of observations must be less than the dataset rows.") }
        val firstTile = Tile(scala.Double.NegativeInfinity, scala.Double.NegativeInfinity, scala.Double.PositiveInfinity, scala.Double.PositiveInfinity)
        val tiles: List[Tile] = cutWithMaxObservations(dataset, firstTile, maxObservations, cutDirectionFunction)
        return new TessellationTree(tiles, tileRadius)
    }

    /*def createWithTilesNumber(dataset: DenseMatrix[Double], tileRadius: Double = 0, tilesNumber: Int): TessellationTree = {

    }*/

    def fromCSV(filePath: String): TessellationTree = {
        var tilesDenseMatrix = csvread(new File(filePath))
        if (tilesDenseMatrix.cols != 4) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree") }
        if (tilesDenseMatrix(0, 0) != sum(tilesDenseMatrix(0, ::))) { throw new IndexOutOfBoundsException("The file is not formatted to be a tessellation tree") }

        val tileRadius = tilesDenseMatrix(0, 0)
        val tiles = ListBuffer.empty[Tile]
        var i = 0
        for (i <- 1 until tilesDenseMatrix.rows) {
            tiles += Tile(tilesDenseMatrix(i, 0), tilesDenseMatrix(i, 1), tilesDenseMatrix(i, 2), tilesDenseMatrix(i, 3))
        }
        return new TessellationTree(tiles.toList, tileRadius)
    }

private def observationsInTile(matrix: DenseMatrix[Double], tile: Tile, axis: Int): DenseMatrix[Double] = {
    var observations = DenseMatrix.zeros[Double](matrix.rows, matrix.cols)
    var numberOfObservations = 0
    for (row <- matrix(*,::)) {
        if (tile.has(row)) {
            observations(numberOfObservations, 0) = row(0)
            observations(numberOfObservations, 1) = row(1)
            numberOfObservations += 1
        }
    }
    return observations(::, 0 until numberOfObservations)
}

private def cutWithMaxObservations(matrix: DenseMatrix[Double], parentTile: Tile, maxObservations: Int, cutDirectionFunction: (Tile, DenseMatrix[Double]) => Boolean): List[Tile] = {
    val observations = observationsInTile(matrix, parentTile, 0)
    if (observations.rows > maxObservations) {
        if (cutDirectionFunction(parentTile, observations)) {
            val medianX = median(observations(::, 0))
            val tileLeft = Tile(parentTile.minX, parentTile.minY, medianX, parentTile.maxY)
            val tileRight = Tile(medianX, parentTile.minY, parentTile.maxX, parentTile.maxY)
            // We use matrix instead of observations to introduce neighborhoods later.
            return List.concat(cutWithMaxObservations(matrix, tileLeft, maxObservations, cutDirectionFunction), cutWithMaxObservations(matrix, tileRight, maxObservations, cutDirectionFunction))
        } else { // Dividing the title horizontally makes more sense.
            val medianY = median(observations(::, 1))
            val tileBottom = Tile(parentTile.minX, parentTile.minY, parentTile.maxX, medianY)
            val tileTop = Tile(parentTile.minX, medianY, parentTile.maxX, parentTile.maxY)
            return List.concat(cutWithMaxObservations(matrix, tileBottom, maxObservations, cutDirectionFunction), cutWithMaxObservations(matrix, tileTop, maxObservations, cutDirectionFunction))
        }
    } else {
        return List(parentTile)
    }
}
}
