package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import java.io.File
import scala.util.control.Breaks._

case class Tile(minX: Double, minY: Double, maxX: Double, maxY: Double)

class TessellationTree(val tiles: List[Tile]) {
    /*def exportCSV(): File = {

    }*/

    /*def toCSV(): File = {

    }*/


}

object TessellationTree {
    def createWithMaxObservations(dataset: DenseMatrix[Double], maxObservationsPerTile: Int): TessellationTree = {
        val firstTile = Tile(scala.Double.NegativeInfinity, scala.Double.NegativeInfinity, scala.Double.PositiveInfinity, scala.Double.PositiveInfinity)
        val tiles: List[Tile] = cut(dataset, firstTile, maxObservationsPerTile)
        return new TessellationTree(tiles)
    }

    /*def createWithTilesNumber(dataset: DenseMatrix[Double], tilesNumber: Int): TessellationTree = {

    }*/

    /*def createCSV(csv: File): TessellationTree = {

    }*/

    def tessellate(data: DenseMatrix[Double], maxPerTile: Int): DenseMatrix[Double] = {
        val firstTile = Tile(scala.Double.NegativeInfinity, scala.Double.NegativeInfinity, scala.Double.PositiveInfinity, scala.Double.PositiveInfinity)
        val tiles: List[Tile] = cut(data, firstTile, maxPerTile)

        var tessellations = DenseMatrix.zeros[Double](tiles.length, 4)
        for (i <- 0 until tiles.length) {
            val tile = tiles(i)
            tessellations(i, 0) = tile.minX
            tessellations(i, 1) = tile.maxX
            tessellations(i, 2) = tile.minY
            tessellations(i, 3) = tile.maxY
        }

        return tessellations
    }


    // Miscellanous functions for tessellation

    def numberOfObservationsInTile(matrix: DenseMatrix[Double], tile: Tile): Int = {
        var observations = 0
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                observations += 1
            }
        }
        return observations
    }

    def observationsInTile(matrix: DenseMatrix[Double], tile: Tile, axis: Int): DenseVector[Double] = {
        var observations = DenseVector.zeros[Double](matrix.rows)
        var numberOfObservations = 0
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                observations(numberOfObservations) = matrix(row, axis)
                numberOfObservations += 1
            }
        }
        return observations(0 until numberOfObservations)
    }

    def minMax(matrix: DenseMatrix[Double], tile: Tile): (DenseVector[Double], DenseVector[Double]) = {
        var min = DenseVector.fill(2){scala.Double.PositiveInfinity}
        var max = DenseVector.fill(2){scala.Double.NegativeInfinity}
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                if (min(0) > matrix(row, 0)) {
                    min(0) = matrix(row, 0)
                }
                if (min(1) > matrix(row, 1)) {
                    min(1) = matrix(row, 1)
                }
                if (max(0) < matrix(row, 0)) {
                    max(0) = matrix(row, 0)
                }
                if (max(1) < matrix(row, 1)) {
                    max(1) = matrix(row, 1)
                }
            }
        }
        return (min, max)
    }

    def cut(matrix: DenseMatrix[Double], parentTile: Tile, maxPerTile: Int): List[Tile] = {
        // If there are too many observations in the tile, we divide it.
        if (numberOfObservationsInTile(matrix, parentTile) > maxPerTile) {
            val (minCols, maxCols) = minMax(matrix, parentTile)

            if (dist(minCols(0), maxCols(0)) >= dist(minCols(1), maxCols(1))) { // Dividing the tile vertically is better
                val projections = observationsInTile(matrix, parentTile, 0)
                val medianX = median(projections)
                val tileLeft = Tile(parentTile.minX, parentTile.minY, medianX, parentTile.maxY)
                val tileRight = Tile(medianX, parentTile.minY, parentTile.maxX, parentTile.maxY)
                // We use matrix instead of observations to introduce neighborhoods later.
                return List.concat(cut(matrix, tileLeft, maxPerTile), cut(matrix, tileRight, maxPerTile))
            } else { // Dividing the title horizontally makes more sense.
                val projections = observationsInTile(matrix, parentTile, 1)
                val medianY = median(projections)
                val tileBottom = Tile(parentTile.minX, parentTile.minY, parentTile.maxX, medianY)
                val tileTop = Tile(parentTile.minX, medianY, parentTile.maxX, parentTile.maxY)
                return List.concat(cut(matrix, tileBottom, maxPerTile), cut(matrix, tileTop, maxPerTile))
            }
        } else {
            return List(parentTile)
        }
    }

    def dist(a: Double, b: Double): Double = {
        return sqrt(pow(a - b, 2))
    }

    def ratio(a: Double, b: Double): Double = {
        if (a > b) {
            return a / b
        } else {
            return b / a
        }
    }
}
