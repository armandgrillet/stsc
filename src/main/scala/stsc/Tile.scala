package stsc

import breeze.linalg.{BitVector, DenseMatrix, DenseVector, Transpose, *}
import breeze.numerics.abs

/** A tile to contain observations. Can be of any dimensions.
  * BE CAREFUL ABOUT THE EDGES: if you have a Tile(DenseVector(0), DenseVector(10), 0), an observation with x = 0 will be in the tile but NOT a tile with x = 10
  *
  * @constructor create a new tile with a list of minimums, maximums and a border width.
  * @param mins the minimums of the tile in every dimension.
  * @param maxs the maximums of the tile in every dimension.
  */
case class Tile(mins: DenseVector[Double], maxs: DenseVector[Double]) {
    require(mins.length == maxs.length, "mins and maxs of the tile have to be the same")

    val emptyBitVector = BitVector.zeros(mins.length)

    /** Check if an observation is in a tile.
      *
      * @param observation the observation to check, represented as a DenseVector.
      * @return if the tile has the observation.
      */
    def has(observation: DenseVector[Double], borderWidth: Double): Boolean = {
        if (observation.length != mins.length) { throw new IndexOutOfBoundsException("The observation dimension has to be the same as the tile.") }
        if (observation :< (mins :- borderWidth) == emptyBitVector && observation :> (maxs :+ borderWidth) == emptyBitVector && (observation :== (maxs :- borderWidth)) == emptyBitVector) {
            return true
        }
        return false
    }

    /** Check if an observation is deeply in a tile, meaning it is only in tile.
      *
      * @param observation the observation to check, represented as a DenseVector.
      * @return if the tile has the observation deeply in it.
      */
    def hasDeeply(observation: DenseVector[Double], borderWidth: Double): Boolean = {
        if ((observation :< (mins :+ borderWidth)) == emptyBitVector && (observation :> (maxs :- borderWidth)) == emptyBitVector && (observation :== (maxs :- borderWidth)) == emptyBitVector) {
            return true
        }
        return false
    }

    /** The length of a tile in every dimension.
      * @return the tile dimensions has a DenseVector, tile.sizes()(0) is the length of the tile in the first dimension.
      */
    def sizes(): DenseVector[Double] = {
        return abs(maxs - mins)
    }

    /** @return the tile as a Transpose with all the minimums then all the maximums. */
    def asTranspose(): Transpose[DenseVector[Double]] = {
        return DenseVector.vertcat(mins, maxs).t
    }

    def filter(dataset: DenseMatrix[Double], borderWidth: Double): DenseMatrix[Double] = {
        val observations = DenseMatrix.zeros[Double](dataset.rows, dataset.cols)
        var numberOfObservations = 0
        for (row <- dataset(*, ::)) {
            if (has(row, borderWidth)) {
                observations(numberOfObservations, ::) := row.t
                numberOfObservations += 1
            }
        }
        return observations(0 until numberOfObservations, ::)
    }
}
