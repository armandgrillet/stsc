import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import scala.util.control.Breaks._
import scala.collection.immutable.TreeMap

case class Tile(minX: Double, minY: Double, maxX: Double, maxY: Double)

/** stsc (Self-Tuning Spectral Clustering) contains methods to cluster a dataset and to create a tesselation of it. */
package object stsc {
    // Public methods
    def cluster(data: DenseMatrix[Double], tesselation: DenseMatrix[Double] = null, radius: Int = 0, min: Int = 2, max: Int = 6): (Map[Int, Double], DenseVector[Int]) = {
        if (min < 2) {
            throw new IllegalArgumentException("The minimum number of clusters cannot be less than 2.");
        }

        if (min >= max) {
            throw new IllegalArgumentException("The minimum number of clusters needs to be strictly inferior compared to the maximum number of clusters.");
        }

        if (tesselation != null) {
            /** There are two colomns per dimensions in a tesselation tree (one for min, one for max).
            * As one column in the dataset represents one dimension,
            * the number of columns in the tesselation tree is 2 times the number of colums in the dataset.
            */
            if (tesselation.cols != 2 * data.cols) {
                throw new IllegalArgumentException("The tesselation structure is wrong. Expected a dense matrix with " + 2 * data.cols + " columns.");
            } else {
                return cluster(data, min, max)
            }
        } else {
            return cluster(data, min, max)
        }
    }

    private def cluster(dataset: DenseMatrix[Double], min: Int, max: Int): (Map[Int, Double], DenseVector[Int]) = {
        // Centralize and scale the data.
        val meanCols = mean(dataset(::, *)).t.toDenseMatrix
        var matrix = (dataset - vertStack(meanCols, dataset.rows))
        matrix /= breeze.linalg.max(abs(matrix))

        // Compute local scale (step 1).
        val distances = euclideanDistance(matrix)
        val locScale = localScale(distances, 7) // In the original paper we use the 7th neighbor to create a local scale.

        // Build locally scaled affinity matrix (step 2).
        val locallyScaledA = locallyScaledAffinityMatrix(distances, locScale)

        // Build the normalized affinity matrix (step 3)
        val diagonalMatrix = diag(pow(sum(locallyScaledA(*, ::)), -0.5)) // Sum of each row, then power -0.5, then matrix.
        var normalizedA = diagonalMatrix * locallyScaledA * diagonalMatrix

        // We only have a
        var row, col = 0
        for (row <- 0 until normalizedA.rows) {
            for (col <- row + 1 until normalizedA.cols) {
                normalizedA(col, row) = normalizedA(row, col)
            }
        }

        // Compute the largest eigenvectors
        val eigenstuff = eigSym(normalizedA)
        var eigenvectors = DenseMatrix.zeros[Double](eigenstuff.eigenvectors.rows, max)
        val biggestEigenvalues = eigenstuff.eigenvalues(-1 to -max by -1)

        var i = 0
        for (i <- 0 until max) {
            for (row <- 0 until eigenstuff.eigenvectors.rows) {
                eigenvectors(row, i) = eigenstuff.eigenvectors(row, -(1 + i))
            }
        }

        // In cluster_rotate.m originally
        var qualities: Map[Int, Double] = Map()
        var currentEigenvectors = eigenvectors(::, 0 until min)
        var (quality, clusters, rotatedEigenvectors) = paraspectre(currentEigenvectors)
        qualities += (min -> quality)

        var group = 0
        for (group <- min until max) {
            val eigenvectorToAdd = eigenvectors(::, group).toDenseMatrix.t
            currentEigenvectors = DenseMatrix.horzcat(rotatedEigenvectors, eigenvectorToAdd)
            val (tempQuality, tempClusters, tempRotatedEigenvectors) = paraspectre(currentEigenvectors)
            qualities += (group + 1 -> tempQuality)
            rotatedEigenvectors = tempRotatedEigenvectors

            if (tempQuality >= quality - 0.002) {
                quality = tempQuality
                clusters = tempClusters
            }
        }

        // Order the qualities
        val orderedQualities = TreeMap(qualities.toSeq:_*)

        return (orderedQualities, clusters)
    }

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

    // Miscellanous functions for clustering

    def vertStack(matrix: DenseMatrix[Double], iterations: Int): DenseMatrix[Double] = {
        var stack = matrix
        var i = 0
        while (i < iterations - 1) {
            stack = DenseMatrix.vertcat(stack, matrix)
            i += 1
        }
        return stack
    }

    def euclideanDistance(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
        var distanceMatrix = DenseMatrix.zeros[Double](matrix.rows, matrix.rows) // Distance matrix, size rows x rows.
        var distanceVector = DenseVector(0.0).t // The distance vector containing the distance between two vectors.

        (0 until matrix.rows).map{ mainRow =>
            (mainRow + 1 until matrix.rows).map{ secondRow =>
                distanceVector = matrix(mainRow, ::) - matrix(secondRow,::) // Xi - Xj | Yi - Yj
                distanceVector *= distanceVector // (Xi - Xj)² | (Yi - Yj)²
                distanceMatrix(mainRow, secondRow) = sqrt(sum(distanceVector)) // √(Xi - Xj)² + (Yi - Yj)² + ...
                distanceMatrix(secondRow, mainRow) = distanceMatrix(mainRow, secondRow)
            }
        }

        return distanceMatrix
    }

    def localScale(distanceMatrix: DenseMatrix[Double], k: Int): DenseVector[Double] = {
        if (k > distanceMatrix.cols - 1) {
            return max(distanceMatrix(*, ::)) // Maximum distance.
        } else {
            var localScale = DenseVector.zeros[Double](distanceMatrix.cols)
            var sortedVector = IndexedSeq(0.0)

            (0 until distanceMatrix.cols).map{col =>
                sortedVector = distanceMatrix(::, col).toArray.sorted
                localScale(col) = sortedVector(k) // Kth nearest distance., the 0th neighbor is always 0 and sortedVector(1) is the first neighbor
            }

            return localScale
        }
    }

    def locallyScaledAffinityMatrix(distanceMatrix: DenseMatrix[Double], localScale: DenseVector[Double]): DenseMatrix[Double] = {
        var affinityMatrix = DenseMatrix.zeros[Double](distanceMatrix.rows, distanceMatrix.cols) // Distance matrix, size rows x cols.

        (0 until distanceMatrix.rows).map{ row =>
            (row + 1 until distanceMatrix.cols).map{ col =>
                affinityMatrix(row, col) = -scala.math.pow(distanceMatrix(row, col), 2) // -d(si, sj)²
                affinityMatrix(row, col) /= (localScale(row) * localScale(col)) // -d(si, sj)² / lambi * lambj
                affinityMatrix(row, col) = scala.math.exp(affinityMatrix(row, col)) // exp(-d(si, sj)² / lambi * lambj)
                affinityMatrix(col, row) = affinityMatrix(row, col)
            }
        }

        return affinityMatrix
    }

    // Step 5 of the self-tuning spectral clustering algorithm.

    var dims = 0
    var data = 0
    var angles = 0
    var ik, jk = DenseVector.zeros[Int](0)
    var ev = DenseMatrix.zeros[Double](0, 0)

    def paraspectre(eigenvectors: DenseMatrix[Double]): (Double, DenseVector[Int], DenseMatrix[Double]) = {
        dims = eigenvectors.cols
        data = eigenvectors.rows
        angles = (dims * (dims - 1) / 2).toInt
        ik = DenseVector.zeros[Int](angles)
        jk = DenseVector.zeros[Int](angles)
        ev = eigenvectors

        var i, j, k = 0
        for (i <- 0 until dims) {
            for (j <- (i + 1) until dims) {
                ik(k) = i
                jk(k) = j
                k += 1
            }
        }

        val maxIterations = 200

        var nablaJ, quality = 0.0
        var newQuality, old1Quality, old2Quality = 0.0 // Variables to compute the descend through true derivative.
        var qualityUp, qualityDown = 0.0 // Variables to descend through numerical derivative.
        var iter, d = 0
        var evRot = DenseMatrix.zeros[Double](0, 0)

        var theta, thetaNew = DenseVector.zeros[Double](angles)

        quality = evaluateQuality(ev)
        old1Quality = quality
        old2Quality = quality

        breakable {
            for (iter <- 1 to maxIterations) {
                for (d <- 0 until angles) {
                    val alpha = 0.1
                    // move up
                    thetaNew(d) = theta(d) + alpha
                    evRot = rotateGivens(thetaNew)
                    qualityUp = evaluateQuality(evRot)

                    // move down
                    thetaNew(d) = theta(d) - alpha
                    evRot = rotateGivens(thetaNew)
                    qualityDown = evaluateQuality(evRot)

                    // update only if at least one of them is better
                    if( qualityUp > quality || qualityDown > quality){
                        if( qualityUp > qualityDown ){
                            theta(d) = theta(d) + alpha
                            thetaNew(d) = theta(d)
                            quality = qualityUp
                        } else {
                            theta(d) = theta(d) - alpha
                            thetaNew(d) = theta(d)
                            quality = qualityDown
                        }
                    }
                }

                if (iter > 2 && ((quality - old2Quality) < 0.001)) {
                    break
                }
                old2Quality = old1Quality
                old1Quality = quality
            }
        }

        val finalEvRot = rotateGivens(thetaNew)

        if (quality equals Double.NaN) {
            return (0, null, finalEvRot)
        } else {
            val clusts = clusters(finalEvRot)
            return (quality, clusts, finalEvRot)
        }
    }

    def clusters(rotatedEigenvectors: DenseMatrix[Double]): DenseVector[Int] = {
        val absEigenvectors = abs(rotatedEigenvectors)
        return argmax(absEigenvectors(*, ::))
    }

    def evaluateQuality(matrix: DenseMatrix[Double]): Double = {
        // Take the square of all entries and find the max of each row
        var squareMatrix = pow(matrix, 2)
        val maxValues = max(squareMatrix(*, ::)) // Max of each row

        val cost = sum(sum(squareMatrix(*, ::)) / max(squareMatrix(*, ::))) // Sum of (sum of each row divided by max of each row).

        return 1.0 - (cost / data - 1.0) / dims
    }

    def rotateGivens(theta: DenseVector[Double]): DenseMatrix[Double] = {
        val g = uAB(theta, 0, angles - 1)
        return ev * g
    }

    def uAB(theta: DenseVector[Double], a: Int, b: Int): DenseMatrix[Double] = {
        var i, k = 0
        var uab = DenseMatrix.eye[Double](dims)

        if (b < a) {
            return uab
        }

        var tt, uIk = 0.0
        for (k <- a to b) {
            tt = theta(k)
            for (i <- 0 until dims) {
                uIk = uab(i, ik(k)) * cos(tt) - uab(i, jk(k)) * sin(tt)
                uab(i, jk(k)) = uab(i, ik(k)) * sin(tt) + uab(i, jk(k)) * cos(tt)
                uab(i, ik(k)) = uIk
            }
        }

        return uab
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
