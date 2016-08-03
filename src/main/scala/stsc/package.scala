package stsc

/** Provides a k-d tree class to cut a dataset and the self-tuning spectral clustering algorithm.
  *
  * ==Overview==
  * The main class to use is the self-tuning spectral clustering algorithm, [[gr.armand.stsc.STSC]]:
  * {{{
  * scala> import gr.armand.stsc.STSC
  * scala> val (clustersQualities, correctClusters) = STSC.cluster(dataset)
  * }}}
  *
  * If you include [[gr.armand.stsc.KDTree]], you can
  * also create a k-d tree to divide a dataset:
  * {{{
  * scala> import gr.armand.stsc.KDTree
  * scala> val tree = KDTree.createWithMaxObservations(dataset, maxObservationsPerTile, tileBorderWidth)
  * }}}
  *
  * The third class of the library is Tile, a list of tiles compose a k-d tree.
  * A Tile is composed of two DenseVectors representing the minimums and maximums in every dimensions.
  *
  * Copyright © 2016 Armand Grillet. All rights reserved.
  */
package object stsc {

}
