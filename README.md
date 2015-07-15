# decision-tree
A decision tree calssifier for use on vectors of real-values.

# Why
This package is intended for those trying to classifying signals from sensors. In particular, this package gives the capability to make

- Trees
- Forests

Additionally, there are a variety of options to speed up the process of finding the best splitting point over each signal. These techniques are coined _threshing_ and include:

- Gaussian
- K-Tile
- Uniform

approximation techniques.

# Plot

This package include a D3 graphic capability known as a _force-graph_. For instance a sample decision tree produced from this package on the iris data set looks like this,

![alt text](images/irisTree.gif)

# Desired Features
Currently this package does not support, but we hope to implement in the future.

1. Pruning Methods

