# K-means Clustering

An implementation of the K-means clustering algorithm from scratch in Gerbil Scheme.

## Overview

Points are represented as fixed-length Gerbil vectors of floats and datasets as lists of those vectors. The library fits a model with `n-init` random restarts and returns the run with the lowest within-cluster sum of squared distances (inertia).

- **Initialization:** k-means++ (default) or uniform random.
- **Assignment step:** each point is assigned to the nearest centroid by squared Euclidean distance.
- **Update step:** each centroid becomes the mean of its assigned points; empty clusters keep their previous centroid.
- **Convergence:** stops when every centroid coordinate moves less than `tol`, or after `max-iters`.
- **Scoring:** inertia (WCSS) picks the best restart and can be inspected on the fitted model.

## Files

| File | Description |
|------|-------------|
| `kmeans-lib.ss` | K-means library (fitting, prediction, distance/init helpers) |
| `test.ss`       | Demo that generates three 2-D Gaussian blobs and clusters them |
| `Makefile`      | `make test` runs the demo |
| `gerbil.pkg`    | Package declaration namespace (`kmeans`) |

## API

- `(kmeans-fit points k max-iters: 100 tol: 1e-6 n-init: 10 init: 'kmeans++)` — Fit a model. Returns a hash table with keys `centroids`, `labels`, `inertia`, `iterations`, `k`, `num-features`.
- `(kmeans-predict model x)` — Return the cluster index for a new point vector `x`.
- `(centroids-of model)` / `(labels-of model)` / `(inertia-of model)` / `(iterations-of model)` — Convenience accessors.
- `(euclidean-distance a b)` / `(squared-distance a b)` — Distance helpers on vectors.
- `(assign-clusters points centroids)` / `(update-centroids ...)` / `(compute-inertia ...)` — Lower-level building blocks.

## Example

```scheme
(import "kmeans-lib")

(def points
  (list (vector 0.0 0.0) (vector 0.1 0.2) (vector -0.1 0.05)
        (vector 5.0 5.0) (vector 5.1 4.9) (vector 4.9 5.2)))

(def model (kmeans-fit points 2))

(displayln (labels-of model))
(displayln (kmeans-predict model (vector 5.0 4.8)))
```

## Running the Demo

```bash
make test
# or equivalently:
gxi test.ss
```

The demo generates three well-separated Gaussian blobs, fits k-means with `k=3`, prints the learned centroids and cluster sizes, and classifies four fresh probe points.

Another test2.ss test:

```
gxi test2.ss
```
