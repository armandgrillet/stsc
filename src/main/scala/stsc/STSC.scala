package stsc

import breeze.linalg.{Axis, DenseMatrix, DenseVector, argmax, max, sum, svd, *}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.{abs, cos, pow, sin, sqrt}
import breeze.optimize._

import scala.collection.immutable.SortedMap
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.Source
import scala.math.exp
import scala.util.control.Breaks.{break, breakable}

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import java.io.File

/** Factory for gr.armand.stsc.STSC instances. */
object STSC {
    /** Cluster a given dataset using a self-tuning spectral clustering algorithm.
    *
    * @param csv the path of the dataset to cluster, each row being an observation with each column representing one dimension
    * @param minClusters the minimum number of clusters in the dataset
    * @param maxClusters the maximum number of clusters in the dataset
    * @return the best possible numer of clusters, a Map of costs (key = number of clusters, value = cost for this number of clusters) and the clusters obtained for the best possible number.
    */
    def cluster(csvPath: String, minClusters: Int = 2, maxClusters: Int = 6): (Int, SortedMap[Int, Double], Array[Int]) = {
        val file = new File(csvPath)
        val matrix = breeze.linalg.csvread(file)

        // Three possible exceptions: empty dataset, minClusters less than 0, minClusters more than maxClusters.
        if (matrix.rows == 0) {
            throw new IllegalArgumentException("The dataset does not contains any observations.")
        }
        if (minClusters < 0) {
            throw new IllegalArgumentException("The minimum number of clusters has to be positive.")
        }
        if (minClusters > maxClusters) {
            throw new IllegalArgumentException("The minimum number of clusters has to be inferior to the maximum number of clusters.")
        }

        return clusterMatrix(matrix, minClusters, maxClusters)
    }

    def sparkCluster(sc: SparkContext, csvPath: String, tilesPath: String, csvOutputPath: String, minTileClusters: Int = 2, maxTileClusters: Int = 6) {
        val tree = KDTree.fromCSV(tilesPath)
        val borderWidth = sc.broadcast(tree.borderWidth)
        val dim = sc.broadcast(tree.dimensions)
        val smallTiles = sc.broadcast(tree.smallTiles.map(_.asArray()))
        val minTiles = sc.broadcast(minTileClusters)
        val maxTiles = sc.broadcast(maxTileClusters)
        val obs = sc.textFile(csvPath)

        val anonymousOrdering = (vS: String) => {
            val v = DenseVector(vS.split(',').map(_.toDouble))
            val inTiles = ArrayBuffer[Array[Double]]()
            for (i <- 0 until smallTiles.value.length) {
                val tile = Tile(new DenseVector(smallTiles.value(i).take(dim.value)), new DenseVector(smallTiles.value(i).takeRight(dim.value)))
                if (tile.has(v, borderWidth.value)) {
                    inTiles += tile.asArray()
                }
            }
            (v, inTiles.toArray)
        }: (DenseVector[Double], Array[Array[Double]])

        val vectorsAndTiles = obs.map(ob => anonymousOrdering(ob))
        val flatVectorsAndTiles = vectorsAndTiles.flatMapValues(identity[Array[Array[Double]]])
        val tiles = flatVectorsAndTiles.groupBy(_._2.toList).values.map(it => (it.map(_._1).toArray, it.head._2)) // map back to requested format

        val anonymousClustering = (vectors: Array[DenseVector[Double]], tileA: Array[Double]) => {
            val tile = Tile(new DenseVector(tileA.take(dim.value)), new DenseVector(tileA.takeRight(dim.value)))
            val matrix = DenseMatrix.zeros[Double](vectors.length, dim.value)
            for (i <- 0 until vectors.length) {
                matrix(i, ::) := vectors(i).t
            }
            val (cBest, costs, clusters) = clusterMatrix(matrix, minTiles.value, maxTiles.value)
            val clustersArray = Array.tabulate(cBest){i => DenseMatrix.zeros[Double](clusters.count(_ == i), dim.value)}
            val counter = Array.fill[Int](cBest)(0)
            for (i <- 0 until matrix.rows) {
                clustersArray(clusters(i))(counter(clusters(i)), ::) := matrix(i, ::)
                counter(clusters(i)) += 1
            }
            val clustersLength = Array.tabulate(cBest)(clustersArray(_).rows)
            val clustersInTile = ArrayBuffer.empty[Int]
            val clustersIndexesInTile = ArrayBuffer.empty[Int]
            for (i <- 0 until clustersLength.length) {
                val clusterCenter = sum(clustersArray(i), Axis._0) :/ clustersLength(i).toDouble
                if (tile.has(clusterCenter.t, 0)) {
                    clustersInTile += clustersLength(i)
                    clustersIndexesInTile += i
                }
            }

            val result = new Array[String](clustersInTile.sum)

            for (i <- 0 until clustersIndexesInTile.length) {
                val start = clustersInTile.slice(0, i).sum
                for (j <- 0 until clustersInTile(i)) {
                    result(start + j) = clustersArray(clustersIndexesInTile(i))(j, ::).t.toArray.mkString(", ") + ", " + i.toString + ".0"
                }
            }

            (result)
        }: Array[String]

        val clusters = tiles.map(tile => anonymousClustering(tile._1, tile._2))
        clusters.map(obs => obs.mkString("\n")).saveAsTextFile(csvOutputPath)
        //val (orderedObs, clusts) = clusters.reduce((obs1, obs2) => (DenseMatrix.vertcat(obs1._1, obs2._1), obs1._2 ++ obs2._2))
        //println(orderedObs)
        // println(orderedMatrix)
        // println(clusters)
    }

    private[stsc] def clusterMatrix(matrix: DenseMatrix[Double], minClusters: Int = 2, maxClusters: Int = 6): (Int, SortedMap[Int, Double], Array[Int]) = {
        // Compute local scale (step 1).
        val distances = euclideanDistances(matrix)
        val scale = localScale(distances, 7) // In the original paper we use the 7th neighbor to create a local scale.

        // Build locally scaled affinity matrix (step 2).
        val scaledMatrix = locallyScaledAffinityMatrix(distances, scale)

        // Build the normalized affinity matrix (step 3)
        val normalizedMatrix = normalizedAffinityMatrix(scaledMatrix)

        // Compute the largest eigenvectors (step 4)
        val largestEigenvectors = svd(normalizedMatrix).leftVectors(::, 0 until maxClusters)

        var cBest = minClusters // The best group number.
        var currentEigenvectors = largestEigenvectors(::, 0 until minClusters) // We only take the eigenvectors needed for the number of clusters.
        var (cost, rotatedEigenvectors) = bestRotation(currentEigenvectors)
        var costs = Map(minClusters -> cost) // List of the costs.
        var bestRotatedEigenvectors = rotatedEigenvectors // The matrix of rotated eigenvectors having the minimal cost.

        for (k <- minClusters until maxClusters) { // We get the cost of stsc for each possible number of clusters.
            val eigenvectorToAdd = largestEigenvectors(::, k).toDenseMatrix.t // One new eigenvector at each turn.
            currentEigenvectors = DenseMatrix.horzcat(rotatedEigenvectors, eigenvectorToAdd) // We add it to the already rotated eigenvectors.
            val (tempCost, tempRotatedEigenvectors) = bestRotation(currentEigenvectors)
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
        val z = argmax(absoluteRotatedEigenvectors(*, ::)).toArray // The alignment result (step 8), conversion to array due to https://issues.scala-lang.org/browse/SI-9578
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

        for (i <- 0 until normalizedMatrix.rows) {
            for (j <- i + 1 until normalizedMatrix.cols) {
                normalizedMatrix(i, j) = diagonalVector(i) * scaledMatrix(i, j) * diagonalVector(j)
                normalizedMatrix(j, i) = normalizedMatrix(i, j)
            }
        }

        return normalizedMatrix
    }

    //
    /** Step 5 of the self-tuning spectral clustering algorithm, recover the rotation R which best aligns the eigenvectors.
    *
    * @param eigenvectors the eigenvectors
    * @return the cost of the best rotation and the rotation as a DenseMatrix.
    */
    private[stsc] def bestRotation(eigenvectors: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
        var nablaJ, cost = 0.0 // Variables used to recover the aligning rotation.
        var newCost, old1Cost, old2Cost = 0.0 // Variables to compute the descend through true derivative.

        val bigK = eigenvectors.cols * (eigenvectors.cols - 1) / 2
        var theta, thetaNew = DenseVector.zeros[Double](bigK)

        cost = j(eigenvectors)
        old1Cost = cost
        old2Cost = cost

        breakable {
            for (i <- 0 until 200) { // Max iterations = 200, as in the original paper code.
                for (k <- 0 until theta.length) { // kth entry in the list composed of the (i, j) indexes
                    val alpha = 0.001
                    nablaJ = numericalQualityGradient(eigenvectors, theta, k, alpha)
                    thetaNew(k) = theta(k) - alpha * nablaJ
                    newCost = j(givensRotation(eigenvectors, thetaNew))

                    if (newCost < cost) {
                        theta(k) = thetaNew(k)
                        cost = newCost
                    } else {
                        thetaNew(k) = theta(k)
                    }
                }

                // If the new cost is not that better, we end the rotation.
                if (i > 2 && (old2Cost - cost) < (0.0001 * old2Cost)) {
                    break
                }
                old2Cost = old1Cost
                old1Cost = cost
            }
        }

        val rotatedEigenvectors = givensRotation(eigenvectors, thetaNew) // The rotation using the "best" theta we found.
        return (cost, rotatedEigenvectors)
    }

    /** Return the cost of a given rotation, follow the computation in the original paper code.
    *
    * @param matrix the rotation to analyze
    * @return the cost, the bigger the better (generally less than 1)
    */
    private[stsc] def j(matrix: DenseMatrix[Double]): Double = {
        val squareMatrix = matrix :* matrix
        return sum(sum(squareMatrix(*, ::)) / max(squareMatrix(*, ::))) // Sum of the sum of each row divided by the max of each row.
    }

    private[stsc] def numericalQualityGradient(matrix: DenseMatrix[Double], theta: DenseVector[Double], k: Int, h: Double): Double = {
        theta(k) = theta(k) + h
        val upRotation = givensRotation(matrix, theta)
        theta(k) = theta(k) - 2 * h // -2 * because we cancel the previous operation.
        val downRotation = givensRotation(matrix, theta)
        return (j(upRotation) - j(downRotation)) / ( 2 * h)
    }

    /** Givens rotation of a given matrix
    *
    * @param matrix the matrix to rotate
    * @param theta the angle of the rotation
    * @return the Givens rotation
    */
    private[stsc] def givensRotation(matrix: DenseMatrix[Double], theta: DenseVector[Double]): DenseMatrix[Double] = {
        // Find the coordinate planes (i, j).
        var i, j = 0
        val ij = List.tabulate(theta.length)(_ => {
            j += 1
            if (j >= matrix.cols) {
                i += 1
                j = i + 1
            }
            (i, j)
        })

        val g = DenseMatrix.eye[Double](matrix.cols) // Create an empty identity matrix.

        var tt, uIk = 0.0
        for (k <- 0 until theta.length) {
            tt = theta(k)
            for (i <- 0 until matrix.cols) {
                uIk = g(i, ij(k)._1) * cos(tt) - g(i, ij(k)._2) * sin(tt)
                g(i, ij(k)._2) = g(i, ij(k)._1) * sin(tt) + g(i, ij(k)._2) * cos(tt)
                g(i, ij(k)._1) = uIk
            }
        }
        return matrix * g
    }
}
