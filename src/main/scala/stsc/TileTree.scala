package stsc

object TileTree {
    def fromList(list: List[Tile]): TileTree = {
        return null
    }
}

case class TileTree(value: Tile, left: TileTree = null, right: TileTree = null) {
    def isLeaf: Boolean = {
        if (left == null && right == null) {
            true
        } else {
            false
        }
    }

    def leafs: Int = {
        if (isLeaf) {
            1
        } else {
            left.leafs + right.leafs
        }
    }

    def length: Int = {
        if (isLeaf) {
            1
        } else {
            1 + left.length + right.length
        }
    }
}
