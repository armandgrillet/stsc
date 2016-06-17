package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

/** A tile to contain observations. Can be of any dimensions.
  * BE CAREFUL ABOUT THE EDGES: if you have a Tile(DenseVector(0), DenseVector(10), 0), an observation with x = 0 will be in the tile but NOT a tile with x = 10
  *
  * @constructor create a new tile with a list of minimums, maximums and a border width.
  * @param mins the minimums of the tile in every dimension.
  * @param maxs the maximums of the tile in every dimension.
  * @param borderWidth the border width used when an observation is between two tiles.
  */
case class Tile(mins: DenseVector[Double], maxs: DenseVector[Double], borderWidth: Double) {
    require(mins.length == maxs.length)
    require(borderWidth >= 0)

    val emptyBitVector = BitVector.zeros(mins.length)

    /** Check if an observation is in a tile.
      *
      * @param observation the observation to check, represented as a DenseVector.
      * @return if the tile has the observation.
      */
    def has(observation: DenseVector[Double], borderWidth: Double = borderWidth): Boolean = {
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
    def hasDeeply(observation: DenseVector[Double]): Boolean = {
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
}
