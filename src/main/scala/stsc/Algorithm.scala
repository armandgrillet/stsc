package stsc

import breeze.linalg.{DenseMatrix, DenseVector, argmax, diag, eigSym, max, sum, *}
import breeze.numerics.{abs, cos, pow, sin, sqrt}
import breeze.stats.mean

import scala.collection.immutable.TreeMap
import scala.math.{max => _} // Remove this import to use breeze.linalg.max instead.
import scala.util.control.Breaks.{break, breakable}

/** Factory for gr.armand.stsc.Algorithm instances. */
object Algorithm {
    /** Cluster a given dataset using a self-tuning spectral clustering algorithm.
    *
    * @param dataset the dataset to cluster, each row being an observation with each column representing one dimension
    * @param minClusters the minimum number of clusters in the dataset
    * @param maxClusters the maximum number of clusters in the dataset
    * @return a Map of qualities (key = number of clusters, value = quality for this number of clusters) and the clusters for the best quality
    */
    def cluster(dataset: DenseMatrix[Double], minClusters: Int = 2, maxClusters: Int = 6): (Map[Int, Double], DenseVector[Int]) = {
        // Three possible exceptions: empty dataset, minClusters less than 0, minClusters more than maxClusters.
        if (dataset.rows == 0) {
            throw new IllegalArgumentException("The dataset does not contains any observations.")
        }
        if (minClusters < 0) {
            throw new IllegalArgumentException("The minimum number of clusters has to be more than 0.")
        }
        if (minClusters > maxClusters) {
            throw new IllegalArgumentException("The minimum number of clusters has to be inferior compared to the maximum number of clusters.")
        }

        // Centralize and scale the data.
        val meanCols = mean(dataset(::, *)) // Create a dense matrix containing the mean of each column
        val tempMatrix = DenseMatrix.tabulate(dataset.rows, dataset.cols) { // A temporary matrix representing the dataset - the mean of each column.
            case (i, j) => dataset(i, j) - meanCols(j)
        }
        val maxTempMatrix = max(abs(tempMatrix))
        var matrix = tempMatrix / maxTempMatrix // The centralized matrix.

        // Compute local scale (step 1).
        val distances = euclideanDistance(matrix)
        val scale = localScale(distances, 7) // In the original paper we use the 7th neighbor to create a local scale.

        // Build locally scaled affinity matrix (step 2).
        val scaledMatrix = locallyScaledAffinityMatrix(distances, scale)
        // Build the normalized affinity matrix (step 3)
        val diagScaledMatrix = diag(pow(sum(scaledMatrix(*, ::)), -0.5)) // Sum of each row, then power -0.5, then matrix.
        var normalizedMatrix = diagScaledMatrix * scaledMatrix * diagScaledMatrix

        var row, col = 0
        for (row <- 0 until normalizedMatrix.rows) {
            for (col <- row + 1 until normalizedMatrix.cols) {
                normalizedMatrix(col, row) = normalizedMatrix(row, col)
            }
        }

        // val diagonalVector = pow(sum(scaledMatrix(*, ::)), -0.5) // Sum of each row, then power -0.5, then matrix.
        // var normalizedMatrix2 = DenseMatrix.zeros[Double](scaledMatrix.rows, scaledMatrix.cols)
        //
        // for (row <- 0 until normalizedMatrix2.rows) {
        //     for (col <- row + 1 until normalizedMatrix2.cols) {
        //         normalizedMatrix2(row, col) = diagonalVector(row) * scaledMatrix(row, col) * diagonalVector(row)
        //         normalizedMatrix2(col, row) = normalizedMatrix2(row, col)
        //     }
        // }
        //
        // println(normalizedMatrix2)

        // Compute the largest eigenvectors
        val eigenvectors = eigSym(normalizedMatrix).eigenvectors // Get the eigenvectors of the normalized affinity matrix.
        val largestEigenvectors = DenseMatrix.tabulate(eigenvectors.rows, maxClusters) {
            case (i, j) => eigenvectors(i, -(1 + j)) // Reverses the matrix to get the largest eigenvectors only.
        }

        // In cluster_rotate.m originally
        var qualities: Map[Int, Double] = Map()
        var clusters = DenseVector(0)

        var currentEigenvectors = largestEigenvectors(::, 0 until minClusters)
        var (quality, rotatedEigenvectors) = stsc(currentEigenvectors)
        qualities += (minClusters -> quality) // Add the quality to the map.

        var group = 0
        for (group <- minClusters until maxClusters) {
            val eigenvectorToAdd = largestEigenvectors(::, group).toDenseMatrix.t
            currentEigenvectors = DenseMatrix.horzcat(rotatedEigenvectors, eigenvectorToAdd)
            val (tempQuality, tempRotatedEigenvectors) = stsc(currentEigenvectors)
            qualities += (group + 1 -> tempQuality)
            rotatedEigenvectors = tempRotatedEigenvectors

            if (tempQuality >= quality - 0.002) {
                quality = tempQuality
                val absoluteRotatedEigenvectors = abs(rotatedEigenvectors)
                clusters = argmax(absoluteRotatedEigenvectors(*, ::))
            }
        }

        // Order the qualities
        val orderedQualities = TreeMap(qualities.toSeq:_*)
        println(orderedQualities)
        return (orderedQualities, clusters)
    }

    /** Returns the euclidean distance of a given dense matrix.
    *
    * @param matrix the matrix that needs to be analyzed, each row being an observation with each column representing one dimension
    * @return the euclidean distance as a dense matrix
    */
    private[stsc] def euclideanDistance(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
        var distanceMatrix = DenseMatrix.zeros[Double](matrix.rows, matrix.rows) // Distance matrix, size rows x rows.
        var distanceVector = DenseVector(0.0).t // The distance vector containing the distance between two vectors.

        var i, j = 0
        for (i <- 0 until matrix.rows) {
            for (j <- i + 1 until matrix.rows) {
                distanceVector = matrix(i, ::) - matrix(j, ::) // Xi - Xj | Yi - Yj
                distanceVector *= distanceVector // (Xi - Xj)² | (Yi - Yj)²
                distanceMatrix(i, j) = sqrt(sum(distanceVector)) // √(Xi - Xj)² + (Yi - Yj)² + ...
                distanceMatrix(j, i) = distanceMatrix(i, j) // Symmetric matrix.
            }
        }

        return distanceMatrix
    }

    /** Returns the local scale as defined in the original paper, a vector containing the Kth nearest neighbor for each observation.
    *
    * @param distanceMatrix the distance matrix used to get the Kth nearest neighbor
    * @param k k, always 7 in the original paper
    * @return the local scale, the dictance of the Kth nearest neighbor for each observation as a dense vector
    */
    private[stsc] def localScale(distanceMatrix: DenseMatrix[Double], k: Int): DenseVector[Double] = {
        if (k > distanceMatrix.cols - 1) {
            throw new IllegalArgumentException("Not enough observations (" + distanceMatrix.cols + ") for k (" + k + ").")
        } else {
            var localScale = DenseVector.zeros[Double](distanceMatrix.cols)
            var sortedVector = IndexedSeq(0.0)

            var i = 0
            for (i <- 0 until distanceMatrix.cols) {
                sortedVector = distanceMatrix(::, i).toArray.sorted // Ordered distances.
                localScale(i) = sortedVector(k) // Kth nearest distance, the 0th neighbor is always 0 and sortedVector(1) is the first neighbor
            }

            return localScale
        }
    }

    /** Returns a locally scaled affinity matrix using a distance matrix and a local scale
    *
    * @param distanceMatrix the distance matrix
    * @param localScale the local scale, the dictance of the Kth nearest neighbor for each observation as a dense vector
    * @return the locally scaled affinity matrix
    */
    private def locallyScaledAffinityMatrix(distanceMatrix: DenseMatrix[Double], localScale: DenseVector[Double]): DenseMatrix[Double] = {
        var affinityMatrix = DenseMatrix.zeros[Double](distanceMatrix.rows, distanceMatrix.cols) // Distance matrix, size rows x cols.

        var i, j = 0
        for (i <- 0 until distanceMatrix.rows) {
            for (j <- i + 1 until distanceMatrix.rows) {
                affinityMatrix(i, j) = -scala.math.pow(distanceMatrix(i, j), 2) // -d(si, sj)²
                affinityMatrix(i, j) /= (localScale(i) * localScale(j)) // -d(si, sj)² / lambi * lambj
                affinityMatrix(i, j) = scala.math.exp(affinityMatrix(i, j)) // exp(-d(si, sj)² / lambi * lambj)
                affinityMatrix(j, i) = affinityMatrix(i, j)
            }
        }

        return affinityMatrix
    }

    //
    /** Step 5 of the self-tuning spectral clustering algorithm, recovery the rotation R whiwh best align the eigenvectors.
    *
    * @param eigenvectors the eigenvectors
    * @return the quality of the best rotation and the linked dense matrix.
    */
    private def stsc(eigenvectors: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
        var nablaJ, quality = 0.0 // Variables used to recover the aligning rotation.
        var newQuality, old1Quality, old2Quality = 0.0 // Variables to compute the descend through true derivative.
        var qualityUp, qualityDown = 0.0 // Variables to descend through numerical derivative.

        var evRot = DenseMatrix.zeros[Double](0, 0)

        var theta, thetaNew = DenseVector.zeros[Double](angles(eigenvectors))

        quality = evaluateQuality(eigenvectors)
        old1Quality = quality
        old2Quality = quality

        var i, j = 0
        breakable {
            for (i <- 1 to 200) { // Max iterations = 200, as in the original paper.
                for (j <- 0 until angles(eigenvectors)) {
                    val alpha = 0.1
                    // move up
                    thetaNew(j) = theta(j) + alpha
                    evRot = rotateGivens(eigenvectors, thetaNew)
                    qualityUp = evaluateQuality(evRot)

                    // move down
                    thetaNew(j) = theta(j) - alpha
                    evRot = rotateGivens(eigenvectors, thetaNew)
                    qualityDown = evaluateQuality(evRot)

                    // update only if at least one of them is better
                    if (qualityUp > quality || qualityDown > quality) {
                        if (qualityUp > qualityDown) {
                            theta(j) = theta(j) + alpha
                            thetaNew(j) = theta(j)
                            quality = qualityUp
                        } else {
                            theta(j) = theta(j) - alpha
                            thetaNew(j) = theta(j)
                            quality = qualityDown
                        }
                    }
                }

                if (i > 2 && ((quality - old2Quality) < 0.001)) {
                    break
                }
                old2Quality = old1Quality
                old1Quality = quality
            }
        }

        val finalEvRot = rotateGivens(eigenvectors, thetaNew)

        if (quality equals Double.NaN) {
            return (0, finalEvRot)
        } else {
            return (quality, finalEvRot)
        }
    }

    private def angles(matrix: DenseMatrix[Double]): Int = {
        return (matrix.cols * (matrix.cols - 1) / 2).toInt
    }

    private def evaluateQuality(matrix: DenseMatrix[Double]): Double = {
        // Take the square of all entries and find the max of each row
        var squareMatrix = matrix :* matrix
        val maxValues = max(squareMatrix(*, ::)) // Max of each row

        val cost = sum(sum(squareMatrix(*, ::)) / max(squareMatrix(*, ::))) // Sum of (sum of each row divided by max of each row).

        return 1.0 - (cost / matrix.rows - 1.0) / matrix.cols
    }

    private def rotateGivens(matrix: DenseMatrix[Double], theta: DenseVector[Double]): DenseMatrix[Double] = {
        val g = uAB(theta, 0, angles(matrix) - 1, matrix.cols, angles(matrix))
        return matrix * g
    }

    private def uAB(theta: DenseVector[Double], a: Int, b: Int, dims: Int, angles: Int): DenseMatrix[Double] = {
        var uab = DenseMatrix.eye[Double](dims)

        if (b < a) {
            return uab
        }

        val ik = DenseVector.zeros[Int](angles)
        val jk = DenseVector.zeros[Int](angles)

        var i, j, k = 0
        for (i <- 0 until dims) {
            for (j <- (i + 1) until dims) {
                ik(k) = i
                jk(k) = j
                k += 1
            }
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
}
