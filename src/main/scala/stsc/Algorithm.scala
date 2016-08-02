package stsc

import breeze.linalg.{DenseMatrix, DenseVector, argmax, max, sum, svd, *}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.{abs, cos, pow, sin, sqrt}
import breeze.optimize._

import scala.collection.immutable.SortedMap
import scala.math.{BigDecimal, exp}
import scala.util.control.Breaks.{break, breakable}

/** Factory for gr.armand.stsc.Algorithm instances. */
object Algorithm {
    /** Cluster a given dataset using a self-tuning spectral clustering algorithm.
    *
    * @param dataset the dataset to cluster, each row being an observation with each column representing one dimension
    * @param minClusters the minimum number of clusters in the dataset
    * @param maxClusters the maximum number of clusters in the dataset
    * @return the best possible numer of clusters, a Map of costs (key = number of clusters, value = cost for this number of clusters) and the clusters for the best cost
    */
    def cluster(dataset: DenseMatrix[Double], minClusters: Int = 2, maxClusters: Int = 6): (Int, Map[Int, Double], DenseVector[Int]) = {
        // Three possible exceptions: empty dataset, minClusters less than 0, minClusters more than maxClusters.
        if (dataset.rows == 0) {
            throw new IllegalArgumentException("The dataset does not contains any observations.")
        }
        if (minClusters < 0) {
            throw new IllegalArgumentException("The minimum number of clusters has to be positive.")
        }
        if (minClusters > maxClusters) {
            throw new IllegalArgumentException("The minimum number of clusters has to be inferior to the maximum number of clusters.")
        }

        // Compute local scale (step 1).
        val distances = euclideanDistances(dataset)
        val scale = localScale(distances, 7) // In the original paper we use the 7th neighbor to create a local scale.

        // Build locally scaled affinity matrix (step 2).
        val scaledMatrix = locallyScaledAffinityMatrix(distances, scale)

        // Build the normalized affinity matrix (step 3)
        val normalizedMatrix = normalizedAffinityMatrix(scaledMatrix)

        // Compute the largest eigenvectors (step 4)
        val largestEigenvectors = svd(normalizedMatrix).leftVectors(::, 0 until maxClusters)

        var cBest = minClusters
        // The clusters, a dense vector where clusters(0) is the cluster where is the first observation.
        var currentEigenvectors = largestEigenvectors(::, 0 until minClusters) // We only take the eigenvectors needed for the number of clusters.
        var (cost, rotatedEigenvectors) = stsc(currentEigenvectors)
        var costs = Map(minClusters -> cost)
        var bestRotatedEigenvectors = rotatedEigenvectors

        var group = 0
        for (k <- minClusters until maxClusters) { // We get the cost of stsc for each possible number of clusters.
            val eigenvectorToAdd = largestEigenvectors(::, k).toDenseMatrix.t // One new eigenvector at each turn.
            currentEigenvectors = DenseMatrix.horzcat(rotatedEigenvectors, eigenvectorToAdd) // We add it to the already rotated eigenvectors.
            val (tempCost, tempRotatedEigenvectors) = stsc(currentEigenvectors)
            costs += (k + 1 -> tempCost) // Add the cost to the map.
            rotatedEigenvectors = tempRotatedEigenvectors // We keep the new rotation of the eigenvectors.

            if (tempCost <= cost * 1.0001) {
                bestRotatedEigenvectors = rotatedEigenvectors
                cBest = k + 1
            }
            if (tempCost < cost) {
                cost = tempCost
            }
        }

        val orderedCosts = SortedMap(costs.toSeq:_*) // Order the costs.
        val absoluteRotatedEigenvectors = abs(bestRotatedEigenvectors)
        val z = argmax(absoluteRotatedEigenvectors(*, ::)) // The alignment result (step 8)
        return (cBest, orderedCosts, z)
    }

    /** Returns the euclidean distances of a given dense matrix.
    *
    * @param matrix the matrix that needs to be analyzed, each row being an observation with each column representing one dimension
    * @return the euclidean distances as a dense matrix
    */
    private[stsc] def euclideanDistances(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
        val distanceMatrix = DenseMatrix.zeros[Double](matrix.rows, matrix.rows) // Distance matrix, size rows x rows.

        for (i <- 0 until matrix.rows) {
            for (j <- i + 1 until matrix.rows) {
                distanceMatrix(i, j) = euclideanDistance(matrix(i, ::).t, matrix(j, ::).t) // breeze.linalg.functions.euclideanDistance
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
            val localScale = DenseVector.zeros[Double](distanceMatrix.cols)

            for (i <- 0 until distanceMatrix.cols) {
                val sortedDistances = distanceMatrix(::, i).toArray.sorted // Ordered distances.
                localScale(i) = sortedDistances(k) // Kth nearest distance, the 0th neighbor is always 0 and sortedVector(1) is the first neighbor
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
    private[stsc] def locallyScaledAffinityMatrix(distanceMatrix: DenseMatrix[Double], localScale: DenseVector[Double]): DenseMatrix[Double] = {
        val affinityMatrix = DenseMatrix.zeros[Double](distanceMatrix.rows, distanceMatrix.cols) // Distance matrix, size rows x cols.

        for (i <- 0 until distanceMatrix.rows) {
            for (j <- i + 1 until distanceMatrix.rows) {
                affinityMatrix(i, j) = -pow(distanceMatrix(i, j), 2) // -d(si, sj)²
                affinityMatrix(i, j) /= (localScale(i) * localScale(j)) // -d(si, sj)² / lambi * lambj
                affinityMatrix(i, j) = exp(affinityMatrix(i, j)) // exp(-d(si, sj)² / lambi * lambj)
                affinityMatrix(j, i) = affinityMatrix(i, j)
            }
        }

        return affinityMatrix
    }

    /** Returns the euclidean distance of a given dense matrix.
    *
    * @param scaledMatrix the matrix that needs to be normalized
    * @return the normalized matrix
    */
    private[stsc] def normalizedAffinityMatrix(scaledMatrix: DenseMatrix[Double]): DenseMatrix[Double] = {
        val diagonalVector = DenseVector.tabulate(scaledMatrix.rows){i => 1 / sqrt(sum(scaledMatrix(i, ::))) } // Sum of each row, then power -0.5.
        val normalizedMatrix = DenseMatrix.zeros[Double](scaledMatrix.rows, scaledMatrix.cols)

        for (row <- 0 until normalizedMatrix.rows) {
            for (col <- row + 1 until normalizedMatrix.cols) {
                normalizedMatrix(row, col) = diagonalVector(row) * scaledMatrix(row, col) * diagonalVector(col)
                normalizedMatrix(col, row) = normalizedMatrix(row, col)
            }
        }

        return normalizedMatrix
    }

    //
    /** Step 5 of the self-tuning spectral clustering algorithm, recovery the rotation R whiwh best align the eigenvectors.
    *
    * @param eigenvectors the eigenvectors
    * @return the cost of the best rotation and the linked dense matrix.
    */
    private[stsc] def stsc(eigenvectors: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
        var nablaJ, cost = 0.0 // Variables used to recover the aligning rotation.
        var newCost, old1Cost, old2Cost = 0.0 // Variables to compute the descend through true derivative.
        var costUp, costDown = 0.0 // Variables to descend through numerical derivative.

        var rotatedEigenvectors = DenseMatrix.zeros[Double](0, 0)

        val bigK = eigenvectors.cols * (eigenvectors.cols - 1) / 2
        var theta, thetaNew = DenseVector.zeros[Double](bigK)

        cost = evaluateCost(eigenvectors)
        old1Cost = cost
        old2Cost = cost

        breakable {
            for (i <- 0 until 200) { // Max iterations = 200, as in the original paper code.
                for (k <- 0 until theta.length) { // kth entry in the list composed of the (i, j) indexes
                    def numericalDerivative() {
                        val alpha = 0.1
                        // Move up.
                        thetaNew(k) = BigDecimal(theta(k) + alpha).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
                        rotatedEigenvectors = rotateGivens(eigenvectors, thetaNew)
                        costUp = evaluateCost(rotatedEigenvectors)

                        // Move down.
                        thetaNew(k) = BigDecimal(theta(k) - alpha).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
                        rotatedEigenvectors = rotateGivens(eigenvectors, thetaNew)
                        costDown = evaluateCost(rotatedEigenvectors)

                        // Update only if at least one of the new cost is better.
                        if (costUp < cost || costDown < cost) {
                            if (costUp < costDown) {
                                theta(k) = theta(k) + alpha
                                thetaNew(k) = theta(k)
                                cost = costUp
                            } else {
                                theta(k) = theta(k) - alpha
                                thetaNew(k) = theta(k)
                                cost = costDown
                            }
                        }
                    }

                    def trueDerivative() {
                        val alpha = 0.46
                        nablaJ = evaluateQualityGradient(theta, k, eigenvectors)
                        thetaNew(k) = theta(k) - alpha * nablaJ
                        rotatedEigenvectors = rotateGivens(eigenvectors, thetaNew)
                        newCost = evaluateCost(rotatedEigenvectors)

                        if (newCost < cost) {
                            theta(k) = thetaNew(k)
                            cost = newCost
                        } else {
                            thetaNew(k) = theta(k)
                        }
                    }

                    def trueNumericalDerivative() {
                        val alpha = 0.001
                        nablaJ = evaluateNumericalQualityGradient(eigenvectors, theta, k, alpha)
                        thetaNew(k) = theta(k) - alpha * nablaJ
                        rotatedEigenvectors = rotateGivens(eigenvectors, thetaNew)
                        newCost = evaluateCost(rotatedEigenvectors)

                        if (newCost < cost) {
                            theta(k) = thetaNew(k)
                            cost = newCost
                        } else {
                            thetaNew(k) = theta(k)
                        }
                    }

                    trueNumericalDerivative()
                }

                // If the new cost is not that better, we end the rotation.
                if (i > 2 && (old2Cost - cost) < (0.0001 * old2Cost)) {
                    break
                }
                old2Cost = old1Cost
                old1Cost = cost
            }
        }

        rotatedEigenvectors = rotateGivens(eigenvectors, thetaNew) // The rotation using the "best" theta we found.
        return (cost, rotatedEigenvectors)
    }

    /** Return the cost of a given rotation, follow the computation in the original paper code.
    *
    * @param matrix the rotation to analyze
    * @return the cost, the bigger the better (generally less than 1)
    */
    private[stsc] def evaluateCost(matrix: DenseMatrix[Double]): Double = {
        var squareMatrix = matrix :* matrix
        return sum(sum(squareMatrix(*, ::)) / max(squareMatrix(*, ::))) // Sum of the sum of each row divided by the max of each row.
    }

    /** Returns a lexicographical list of (i, j) following the third paragraph in the appendix.
    *
    * @param bigK N * (N - 1) / 2 as in the abstract of the stochastic gradient descent paper
    * @param cols number of columns in the matrix we want to index
    * @return the indexes, a list of tuples (i,j). For bigK and cols = 3 it is List((0,1), (0,2), (1,2))
    */
    private[stsc] def indexes(bigK: Int, cols: Int): List[(Int, Int)] = {
        var i, j = 0
        return List.tabulate(bigK)(_ => {
            j += 1
            if (j >= cols) {
                i += 1
                j = i + 1
            }
            (i, j)
        })
    }

    /** Returns the quality gradient (nabla J in the appendix of the paper)
    *
    * @param theta
    * @param k
    * @param matrix
    * @return the quality gradient
    */
    private[stsc] def evaluateQualityGradient(theta: DenseVector[Double], k: Int, matrix: DenseMatrix[Double]): Double = {
        val ij = indexes(theta.length, matrix.cols)
        val entry = ij(k)

        // Build V, U, A
        var vForAngle = DenseMatrix.zeros[Double](matrix.cols, matrix.cols)
        vForAngle(entry._1, entry._1) = -sin(theta(k))
        vForAngle(entry._1, entry._2) = cos(theta(k))
        vForAngle(entry._2, entry._1) = -cos(theta(k))
        vForAngle(entry._2, entry._2) = -sin(theta(k))
        val u1 = uAB(theta, 1, k - 1, matrix.cols)
        val u2 = uAB(theta, k + 1, theta.length -1, matrix.cols)

        val a = matrix * u1 * vForAngle * u2

        val z = rotateGivens(matrix, theta)

        val maxValues = max(z(*, ::)) // Max of each row
        val maxIndexCol = argmax(z(*, ::))

        // Compute gradient
        var nablaJ, tmp1, tmp2 = 0.0
        for (i <- 0 until matrix.rows) { // Loop over all rows
            for (j <- 0 until matrix.cols) { // Loop over all columns
                tmp1 = a(i, j) * z(i, j) / (maxValues(i) * maxValues(i))
                tmp2 = (z(i, j) * z(i, j)) * a(i, maxIndexCol(i)) / pow(maxValues(i), 3)
                nablaJ += tmp1 - tmp2
            }
        }
        nablaJ = 2 * nablaJ / matrix.rows / matrix.cols

        return nablaJ
    }

    private[stsc] def evaluateNumericalQualityGradient(matrix: DenseMatrix[Double], theta: DenseVector[Double], k: Int, h: Double): Double = {
        theta(k) = theta(k) + h
        val upRotation = rotateGivens(matrix, theta)
        theta(k) = theta(k) - 2 * h
        val downRotation = rotateGivens(matrix, theta)
        return (evaluateCost(upRotation) - evaluateCost(downRotation)) / ( 2 * h)
    }

    /** Givens rotation of a given matrix
    *
    * @param matrix the matrix to rotate
    * @param theta the angle of the rotation
    * @return the Givens rotation
    */
    private[stsc] def rotateGivens(matrix: DenseMatrix[Double], theta: DenseVector[Double]): DenseMatrix[Double] = {
        val g = uAB(theta, 0, theta.length - 1, matrix.cols)
        return matrix * g
    }

    /** Build U(a,b) (check appendix A of the original paper for more info)
    *
    * @param theta the angle of the rotation
    * @param a
    * @param b
    * @param dims
    * @return the gradient
    */
    private[stsc] def uAB(theta: DenseVector[Double], a: Int, b: Int, dims: Int): DenseMatrix[Double] = {
        var uab = DenseMatrix.eye[Double](dims) // Create an empty identity matrix.

        if (b < a) {
            return uab
        }

        val ij = indexes(theta.length, dims)

        var tt, uIk = 0.0
        for (k <- a to b) {
            tt = theta(k)
            for (i <- 0 until dims) {
                uIk = uab(i, ij(k)._1) * cos(tt) - uab(i, ij(k)._2) * sin(tt)
                uab(i, ij(k)._2) = uab(i, ij(k)._1) * sin(tt) + uab(i, ij(k)._2) * cos(tt)
                uab(i, ij(k)._1) = uIk
            }
        }

        return uab
    }
}
