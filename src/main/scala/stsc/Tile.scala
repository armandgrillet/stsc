package stsc

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

case class Tile(minX: Double, maxX: Double, minY: Double, maxY: Double) {
    def has(observation: DenseVector[Double]): Boolean = {
        if (observation(0) >= this.minX && observation(0) <= this.maxX && observation(1) >= this.minY && observation(1) <= this.maxY) {
            return true
        }
        return false
    }

    def hasDeeply(observation: DenseVector[Double], radius: Double): Boolean = {
        if (observation(0) + radius >= this.minX && observation(0) - radius <= this.maxX && observation(1) + radius >= this.minY && observation(1) - radius <= this.maxY) {
            return true
        }
        return false
    }

    def dimensions(): (Double, Double) = {
        return (stsc.dist(minX, maxX), stsc.dist(minY, maxY))
    }

    def transpose(): Transpose[DenseVector[Double]] = {
        return DenseVector(this.minX, this.maxX, this.minY, this.maxY).t
    }
}
