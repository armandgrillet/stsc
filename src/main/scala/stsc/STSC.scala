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

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, FileUtil, FSDataOutputStream, FileStatus, Path}
import org.apache.hadoop.io.IOUtils

import java.io.File

/** Factory for gr.armand.stsc.STSC instances. */
object STSC {
    /** Cluster a given dataset using a self-tuning spectral clustering algorithm.
    *
    * @param csv the dataset to cluster, each row being an observation with each column representing one dimension
    * @param minClusters the minimum number of clusters in the dataset
    * @param maxClusters the maximum number of clusters in the dataset
    * @return the best possible numer of clusters, a Map of costs (key = number of clusters, value = cost for this number of clusters) and the clusters obtained for the best possible number.
    */
    def cluster(matrix: DenseMatrix[Double], minClusters: Int = 2, maxClusters: Int = 6): (Int, SortedMap[Int, Double], Array[Int]) = {
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

    /** Cluster a given dataset using a self-tuning spectral clustering algorithm.
    *
    * @param csvPath the path of the dataset to cluster, each row being an observation with each column representing one dimension
    * @param minClusters the minimum number of clusters in the dataset
    * @param maxClusters the maximum number of clusters in the dataset
    * @return the best possible numer of clusters, a Map of costs (key = number of clusters, value = cost for this number of clusters) and the clusters obtained for the best possible number.
    */
    def clusterCSV(csvPath: String, minClusters: Int = 2, maxClusters: Int = 6): (Int, SortedMap[Int, Double], Array[Int]) = {
        val file = new File(csvPath)
        val matrix = breeze.linalg.csvread(file)
        return cluster(matrix, minClusters, maxClusters)
    }

    def sparkCluster(sc: SparkContext, csvPath: String, kdTreePath: String, outputPath: String, onlyCentres: Boolean = false, minTileClusters: Int = 2, maxTileClusters: Int = 6) {
        val tree = KDTree.fromCSV(kdTreePath)
        val treeString = sc.broadcast(tree.toString())

        val smallTiles = sc.broadcast(tree.smallTiles.map(_.toDenseVector()).zipWithIndex.toMap) // Map with smallTiles(tile) being the position of the tile, giving us a unique ID.
        val borderWidth = sc.broadcast(tree.borderWidth)
        val dim = sc.broadcast(tree.dimensions)
        val minClusters = sc.broadcast(minTileClusters)
        val maxClusters = sc.broadcast(maxTileClusters)

        val order = (vS: String) => {
            val v = DenseVector(vS.split(',').map(_.toDouble))
            val tree = KDTree.fromString(treeString.value)
            val owningTiles = tree.owningTiles(v).map(_.toDenseVector())
            Seq.tabulate(owningTiles.length)(i => (smallTiles.value(owningTiles(i)), v))
        }: Seq[(Int, DenseVector[Double])]

        val tilesAndVectors = sc.textFile(csvPath).flatMap(order(_)).groupByKey().map{tAndV => (tAndV._1, tAndV._2.toArray)} // groupByKey returns an iterator, not an array.

        val clusterCentres = (tileID: Int, vectors: Array[DenseVector[Double]]) => {
            var tile = Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0))
            for ((t, id) <- smallTiles.value) {
                if (tileID == id) {
                    tile = Tile(t(0 until dim.value), t(dim.value to -1))
                }
            }
            val matrix = DenseMatrix.zeros[Double](vectors.length, dim.value)
            for (i <- 0 until vectors.length) {
                matrix(i, ::) := vectors(i).t
            }
            val (cBest, costs, clusters) = clusterMatrix(matrix, minClusters.value, maxClusters.value)
            val clustersAndObs = Array.tabulate(vectors.length)(i => (clusters(i), vectors(i))).groupBy(_._1)

            val correctCentres = ArrayBuffer.empty[DenseVector[Double]]

            for ((cluster, obs) <- clustersAndObs) {
                val obsAsDM = DenseMatrix.zeros[Double](obs.length, dim.value)
                for ((ob, i) <- obs.zipWithIndex) {
                    obsAsDM(i, ::) := ob._2.t
                }
                val clusterCenter = SparkIsNotABreeze.sumRows(obsAsDM) :/ obs.length.toDouble
                if (tile.has(clusterCenter.t, 0)) {
                    correctCentres += clusterCenter.t
                }
            }
            correctCentres.toSeq
        }: Seq[DenseVector[Double]]

        val fullCluster = (tileID: Int, vectors: Array[DenseVector[Double]]) => {
            var tile = Tile(DenseVector.zeros[Double](0), DenseVector.zeros[Double](0))
            for ((t, id) <- smallTiles.value) {
                if (tileID == id) {
                    tile = Tile(t(0 until dim.value), t(dim.value to -1))
                }
            }
            val matrix = DenseMatrix.zeros[Double](vectors.length, dim.value)
            for (i <- 0 until vectors.length) {
                matrix(i, ::) := vectors(i).t
            }
            val (cBest, costs, clusters) = clusterMatrix(matrix, minClusters.value, maxClusters.value)
            val clustersAndObs = Array.tabulate(vectors.length)(i => (clusters(i), vectors(i))).groupBy(_._1)

            val result = ArrayBuffer.empty[(DenseVector[Double], Int, Int, DenseMatrix[Double])]

            for ((cluster, obs) <- clustersAndObs) {
                val obsAsDM = DenseMatrix.zeros[Double](obs.length, dim.value)
                for ((ob, i) <- obs.zipWithIndex) {
                    obsAsDM(i, ::) := ob._2.t
                }
                val clusterCenter = SparkIsNotABreeze.sumRows(obsAsDM) :/ obs.length.toDouble
                if (tile.has(clusterCenter.t, 0)) {
                    result += ((clusterCenter.t, cluster, tileID, obsAsDM))
                }
            }
            result.toSeq
        }: Seq[(DenseVector[Double], Int, Int, DenseMatrix[Double])]

        val asJSON = (clusterID: Int, tileID: Int, vectors: DenseMatrix[Double]) => {
            var json = "\t},\n\t{\n\t\t\"id\": \"" + tileID.toString + "." + clusterID.toString + "\",\n\t\t\"obs\": [\n"
            for (i <- 0 until vectors.rows) {
                if (i != 0) { json += ",\n" }
                json += "\t\t\t{ \"coord\": \"" + vectors(i, ::).t.toArray.mkString(", ") + "\" }"
            }
            json += "\n\t\t]"
            json
        }: String

        val asCSV = (clusterID: Int, tileID: Int, vectors: DenseMatrix[Double]) => {
            Array.tabulate(vectors.rows)(i => (vectors(i, ::).t, tileID, clusterID)).toSeq
        }: Seq[(DenseVector[Double], Int, Int)]

        val hadoopConfig = new Configuration()
        val hdfs = FileSystem.get(hadoopConfig)
        if (onlyCentres) {
            FileUtil.fullyDelete(new File("/tmp/result"))
            FileUtil.fullyDelete(new File(outputPath))
            tilesAndVectors.flatMap(tile => clusterCentres(tile._1, tile._2)).map(_.toArray.mkString(", ")).saveAsTextFile("/tmp/result")
            FileUtil.copyMerge(hdfs, new Path("/tmp/result"), hdfs, new Path(outputPath), false, hadoopConfig, null)
            FileUtil.fullyDelete(new File("/tmp/result"))
        } else {
            FileUtil.fullyDelete(new File("/tmp/resultCSV"))
            FileUtil.fullyDelete(new File(outputPath + ".csv"))
            FileUtil.fullyDelete(new File(outputPath + ".json"))
            val clusters = tilesAndVectors.flatMap(tile => fullCluster(tile._1, tile._2))
            clusters.flatMap(cluster => asCSV(cluster._2, cluster._3, cluster._4)).map(ob => ob._1.toArray.mkString(", ") + ", " + ob._2 + "." + ob._3).saveAsTextFile("/tmp/resultCSV")
            FileUtil.copyMerge(hdfs, new Path("/tmp/resultCSV"), hdfs, new Path(outputPath + ".csv"), false, hadoopConfig, null)
            FileUtil.fullyDelete(new File("/tmp/resultCSV"))

            FileUtil.fullyDelete(new File("/tmp/resultJSON"))
            clusters.map(cluster => asJSON(cluster._2, cluster._3, cluster._4)).saveAsTextFile("/tmp/resultJSON")
            val out = hdfs.create(new Path(outputPath + ".json"))
            out.write(("[\n").getBytes("UTF-8"))

            try {
                val folder = hdfs.listStatus(new Path("/tmp/resultJSON"))
                for (i <- 1 until folder.length) { // folder(0) is a useless file.
                    val in = hdfs.open(folder(i).getPath())
                    if (i == 1) {
                        in.skip(("\t},\n").getBytes("UTF-8").length)
                    }
                    try {
                        IOUtils.copyBytes(in, out, hadoopConfig, false)
                    } finally {
                        in.close()
                    }
                }
                out.write(("\t}\n]").getBytes("UTF-8"))
            } finally {
                out.close()
            }
            FileUtil.fullyDelete(new File("/tmp/resultJSON"))
        }
    }

    private[stsc] def clusterMatrix(matrix: DenseMatrix[Double], minClusters: Int, maxClusters: Int): (Int, SortedMap[Int, Double], Array[Int]) = {
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
        val absoluteRotatedEigenvectors = SparkIsNotABreeze.getSomeAbs(bestRotatedEigenvectors)
        val z = argmax(bestRotatedEigenvectors(*, ::)).toArray // The alignment result (step 8), conversion to array due to https://issues.scala-lang.org/browse/SI-9578
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
        val sumVector = SparkIsNotABreeze.sumCols(squareMatrix)
        val maxVector = max(squareMatrix(*, ::))
        return sum(sumVector / maxVector) // Sum of the sum of each row divided by the max of each row.
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
