package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

case class Tile(mins: DenseVector[Double], maxs: DenseVector[Double], borderWidth: Double) {
    def has(observation: DenseVector[Double]): Boolean = {
        if ((observation :< (this.mins :- this.borderWidth)).length == 0 && (observation :> (this.maxs :+ this.borderWidth)).length == 0) {
            return true
        }
        return false
    }

    def hasDeeply(observation: DenseVector[Double]): Boolean = {
        if ((observation :< (this.mins :+ this.borderWidth)).length == 0 && (observation :> (this.maxs :- this.borderWidth)).length == 0) {
            return true
        }
        return false
    }

    def dimensions(): DenseVector[Double] = {
        return abs(this.maxs - this.mins)
    }

    def transpose(): Transpose[DenseVector[Double]] = {
        return DenseVector.vertcat(this.mins, this.maxs).t
    }
}
