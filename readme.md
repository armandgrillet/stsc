# stsc

<p align="center">
<img src="stsc.png">
</p>

A implementation of the Self-Tuning Spectral Clustering algorithm, and more. Based on the paper [*Self Tuning Spectral Clustering Algorithm*](http://www.vision.caltech.edu/lihi/Demos/SelfTuningClustering.html).

## Overview
The main class to use is the self-tuning spectral clustering algorithm, `gr.armand.stsc.Algorithm`:

````scala
scala> import gr.armand.stsc.Algorithm
scala> val (clustersQualities, correctClusters) = Algorithm.cluster(dataset)
````
If you include `gr.armand.stsc.TessellationTree`, you can
also create a tessellation tree to divide a dataset:

````scala
scala> import gr.armand.stsc.TessellationTree
scala> val tree = TessellationTree.createWithMaxObservations(dataset, maxObservationsPerTile, tileBorderWidth)
````

The third class of the library is `Tile`, a list of tiles compose a tessellation tree.
A Tile is composed of two DenseVectors representing the minimums and maximums in every dimensions.

The Scaladoc can be found [here](https://armand.gr/stsc).

Copyright © 2016 Armand Grillet. All rights reserved.
