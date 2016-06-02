package stsc

/** Provides a tesselation tree class to cut a dataset and the self-tuning spectral clustering algorithm.
  *
  * ==Overview==
  * The main class to use is the self-tuning spectral clustering algorithm, [[gr.armand.stsc.Algorithm]]:
  * {{{
  * scala> import gr.armand.stsc.Algorithm
  * scala> val (clustersQualities, correctClusters) = Algorithm.cluster(dataset)
  * }}}
  *
  * If you include [[gr.armand.stsc.TessellationTree]], you can
  * also create a tessellation tree to divide a dataset:
  * {{{
  * scala> import gr.armand.stsc.TessellationTree
  * scala> val tree = TessellationTree.createWithMaxObservations(dataset, maxObservationsPerTile, tileBorderWidth)
  * }}}
  *
  * The third class of the library is Tile, a list of tiles compose a tessellation tree.
  * A Tile is composed of two DenseVectors representing the minimums and maximums in every dimensions.
  *
  * Copyright © 2016 Armand Grillet. All rights reserved.
  */
package object stsc {

}
