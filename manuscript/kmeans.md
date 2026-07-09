# K-means Clustering

The previous chapter on Gaussian anomaly detection asked "does this observation look like the ones I have already seen?". K-means clustering asks a different unsupervised question: "if I had to sort these observations into `k`$ groups, what would the most natural grouping be?" Neither question requires labelled training data. Both are workhorses of exploratory data analysis, customer segmentation, image compression, document organization, and preprocessing for downstream supervised models.

In this chapter we build a K-means clustering library from scratch in Gerbil Scheme. The library supports two initialization strategies, multiple random restarts, an inertia-based scoring criterion for picking the best restart, and both `fit` and `predict` operations. We demonstrate it on a synthetic dataset of three Gaussian blobs in the plane, so that the "correct answer" is obvious to the eye and easy to check against the library's output.

## Theoretical Background

Given a dataset of `n`$ points in `d`$-dimensional space, K-means partitions the points into `k`$ clusters, where each cluster is represented by a single point called its **centroid**. The algorithm assigns each observation to the cluster whose centroid is nearest (by squared Euclidean distance) and then moves each centroid to the mean of its assigned points. These two steps are repeated until the centroids stop moving.

Formally, K-means seeks the assignment of points `x_i`$ to clusters `C_j`$ that minimizes the **within-cluster sum of squared distances**, often called the **inertia** or WCSS:

```$
J = \sum_{j=1}^{k} \sum_{x_i \in C_j} \| x_i - \mu_j \|^2
```

where `\mu_j`$ is the centroid of cluster `C_j`$. The iterative algorithm is guaranteed to decrease `J`$ at every step (both the assignment step and the update step can only lower it) and converges to a local minimum. It is not guaranteed to find the global minimum, which is why we run several restarts and keep the best one.

The classic K-means training loop looks like this:

1. **Initialize** `k`$ centroids somewhere reasonable.
2. **Assign** each point to the nearest centroid.
3. **Update** each centroid to be the mean of its assigned points.
4. **Repeat** steps 2 and 3 until no centroid moves by more than a small tolerance, or a maximum iteration count is reached.
5. **Report** the final assignment and the inertia `J`$.

Two well-known pitfalls of K-means are worth handling explicitly. First, a purely random initialization can start with two centroids near each other and end up in a poor local minimum. The **k-means++** initialization scheme, due to Arthur and Vassilvitskii (2007), addresses this by picking the first centroid uniformly at random and each subsequent centroid with probability proportional to the squared distance from the nearest already-chosen centroid, biasing the initial centroids to be spread out. Second, an "empty cluster" can appear if no point is nearest to some centroid after the assignment step. Our implementation handles this by keeping the previous centroid in that slot.

## The Dataset

For the demo we generate synthetic data so that ground truth is known. Each point is a 2-D vector sampled from one of three Gaussian distributions with these true centers and a shared standard deviation of `0.35`$:

```
Blob A: 60 points around (0.0, 0.0)
Blob B: 60 points around (4.0, 0.0)
Blob C: 60 points around (2.0, 3.5)
```

The blobs are well separated (they are several standard deviations apart), so a correct implementation should recover the three centers to within a few hundredths in each coordinate and place exactly 60 points in each cluster. Sample points from one blob might look like:

```
#(-0.041  0.194)
#( 0.221 -0.310)
#( 0.077  0.052)
```

Vectors of fixed length are the natural representation for feature points in Gerbil Scheme: they give constant-time indexed access and are the same shape used by the anomaly detection module in the previous chapter, keeping the code style consistent.

## Project Structure

The project directory `source_code/Kmeans` contains:

| File | Description |
|------|-------------|
| `kmeans-lib.ss` | K-means library: distance helpers, initializers, fit, predict, accessors. |
| `test.ss` | Demo that generates three Gaussian blobs and clusters them. |
| `Makefile` | Single `test` target that invokes `gxi test.ss`. |
| `gerbil.pkg` | Package declaration (`kmeans`). |
| `README.md` | Short quick-reference for the library. |

## The K-means Library: kmeans-lib.ss

This module is self-contained. Its only imports are `:std/format` for formatted output and `:gerbil/gambit` for runtime primitives such as `random-real`. Everything the library needs to fit and predict is exported so that a demo or downstream application can reach in at whatever level of granularity is convenient.

```scheme
;; File: kmeans-lib.ss
;; K-means clustering library
(import :std/format
        :gerbil/gambit)

(export euclidean-distance
        squared-distance
        assign-clusters
        update-centroids
        compute-inertia
        initialize-centroids-random
        initialize-centroids-plusplus
        kmeans-fit
        kmeans-predict
        centroids-of
        labels-of
        inertia-of
        iterations-of
        format-decimal)
```

### Decimal Formatting Helpers

Gerbil's default number printing produces long decimal expansions like `0.03497348274`. For readable reports we reuse the same `format-decimal` idiom used in the anomaly detection chapter, which rounds to a fixed number of places and pads with zeros:

```scheme
;; Find index of character in string (helper for format-decimal)
(def (string-index str char)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) char) i)
        (else (loop (+ i 1)))))))

;; Format floating point numbers to a fixed number of decimal places
(def (format-decimal x places)
  (let* ((multiplier (expt 10 places))
         (rounded (/ (round (* x multiplier)) multiplier))
         (str (number->string (exact->inexact rounded))))
    (let ((dot-pos (string-index str #\.)))
      (let ((formatted-str
             (if dot-pos
               (let* ((frac-len (- (string-length str) dot-pos 1))
                      (diff (- places frac-len)))
                 (if (> diff 0)
                   (string-append str (make-string diff #\0))
                   (substring str 0 (+ dot-pos places 1))))
               (string-append str "." (make-string places #\0)))))
        (if (char=? (string-ref formatted-str 0) #\.)
          (string-append "0" formatted-str)
          formatted-str)))))
```

The call to `exact->inexact` guarantees we always get a decimal string rather than a rational literal like `"7/2"` for numbers that happen to be exact.

### Squared Distance and Euclidean Distance

K-means only ever compares distances, so the square root in Euclidean distance is redundant during clustering. We split the two operations so that the assignment and inertia loops can call the cheaper `squared-distance` while user code that wants a real distance value can call `euclidean-distance`:

```scheme
;; Squared Euclidean distance between two feature vectors
(def (squared-distance a b)
  (let ((n (vector-length a))
        (sum-val 0.0))
    (do ((i 0 (+ i 1)))
        ((= i n) sum-val)
      (let ((d (- (vector-ref a i) (vector-ref b i))))
        (set! sum-val (+ sum-val (* d d)))))))

;; Euclidean distance
(def (euclidean-distance a b)
  (sqrt (squared-distance a b)))
```

`squared-distance` walks the two vectors coordinate by coordinate, accumulates the squared differences, and returns the running total. This yields `\sum_i (a_i - b_i)^2`$, which is `\| a - b \|^2`$.

### Nearest-Centroid Lookup

The assignment step reduces to "for each point, find the index of the closest centroid". Rather than allocating a list of distances, we sweep once through the centroid vector and remember the best index found so far:

```scheme
;; Find index of nearest centroid to point x
(def (nearest-centroid-index x centroids)
  (let ((k (vector-length centroids))
        (best-i 0)
        (best-d 1.0e30))
    (do ((i 0 (+ i 1)))
        ((= i k) best-i)
      (let ((d (squared-distance x (vector-ref centroids i))))
        (when (< d best-d)
          (set! best-d d)
          (set! best-i i))))))
```

The initial `best-d` of `1.0e30` is a sentinel value that is guaranteed to be replaced on the first iteration of the loop.

### The Assignment Step

`assign-clusters` is a thin wrapper that calls `nearest-centroid-index` for every point and stores the result in a label vector:

```scheme
;; Assign each point to its nearest centroid.
;; points is a list of vectors; returns a vector of integer labels.
(def (assign-clusters points centroids)
  (let* ((n (length points))
         (labels (make-vector n 0)))
    (let loop ((ps points) (i 0))
      (if (null? ps)
        labels
        (begin
          (vector-set! labels i (nearest-centroid-index (car ps) centroids))
          (loop (cdr ps) (+ i 1)))))))
```

Points come in as a list because it is convenient for the demo to `append` blobs together with the list primitive, but labels come out as a vector because we index into them by position immediately afterwards.

### The Update Step

The update step computes, for each cluster, the mean of its assigned points and installs that as the new centroid. If a cluster receives no points during this pass, we hold onto the previous centroid so the algorithm can continue rather than crashing on a divide-by-zero:

```scheme
;; Recompute centroids as the mean of assigned points.
;; If a cluster is empty, keep the previous centroid.
(def (update-centroids points labels k num-features prev-centroids)
  (let ((sums (make-vector k #f))
        (counts (make-vector k 0))
        (new-centroids (make-vector k #f)))
    (do ((c 0 (+ c 1)))
        ((= c k))
      (vector-set! sums c (make-vector num-features 0.0)))
    (let loop ((ps points) (i 0))
      (unless (null? ps)
        (let* ((label (vector-ref labels i))
               (bucket (vector-ref sums label))
               (pt (car ps)))
          (do ((f 0 (+ f 1)))
              ((= f num-features))
            (vector-set! bucket f (+ (vector-ref bucket f) (vector-ref pt f))))
          (vector-set! counts label (+ (vector-ref counts label) 1)))
        (loop (cdr ps) (+ i 1))))
    (do ((c 0 (+ c 1)))
        ((= c k) new-centroids)
      (let ((cnt (vector-ref counts c)))
        (if (= cnt 0)
          (vector-set! new-centroids c (vector-copy (vector-ref prev-centroids c)))
          (let ((mean (make-vector num-features 0.0))
                (bucket (vector-ref sums c)))
            (do ((f 0 (+ f 1)))
                ((= f num-features))
              (vector-set! mean f (/ (vector-ref bucket f) cnt)))
            (vector-set! new-centroids c mean)))))))
```

The two-pass structure (accumulate then divide) is the standard streaming way to compute a mean without materializing the sublists of points per cluster.

### Inertia and Convergence

Inertia is simply the sum of the squared distances from every point to its assigned centroid. We use it both as the scoring criterion between random restarts and as an informative quality metric to report to the user:

```scheme
;; Within-cluster sum of squared distances (inertia).
(def (compute-inertia points labels centroids)
  (let ((sum-val 0.0))
    (let loop ((ps points) (i 0))
      (if (null? ps)
        sum-val
        (let* ((label (vector-ref labels i))
               (d (squared-distance (car ps) (vector-ref centroids label))))
          (set! sum-val (+ sum-val d))
          (loop (cdr ps) (+ i 1)))))))
```

Convergence is decided by comparing every coordinate of every centroid against its previous value. If all of them have moved less than `tol`, we consider the run converged:

```scheme
;; True if two centroid sets are within tol of each other on every coordinate.
(def (centroids-converged? old-centroids new-centroids tol)
  (let ((k (vector-length old-centroids))
        (converged #t))
    (do ((c 0 (+ c 1)))
        ((or (not converged) (= c k)) converged)
      (let* ((a (vector-ref old-centroids c))
             (b (vector-ref new-centroids c))
             (nf (vector-length a)))
        (do ((f 0 (+ f 1)))
            ((or (not converged) (= f nf)))
          (when (> (abs (- (vector-ref a f) (vector-ref b f))) tol)
            (set! converged #f)))))))
```

Both loops short-circuit via the `converged` flag as soon as a large enough difference is found, so most non-converged runs finish this check very quickly.

### Random Initialization

The simpler of the two initializers picks `k`$ distinct data points uniformly at random and uses their coordinates as the starting centroids. A hash table of already-used indices keeps the picks distinct:

```scheme
;; Uniform random selection of k distinct starting points.
(def (initialize-centroids-random points k)
  (let* ((point-vec (list->vector points))
         (n (vector-length point-vec))
         (chosen (make-vector k #f))
         (picked-count 0)
         (used (make-hash-table)))
    (let loop ()
      (when (< picked-count k)
        (let ((idx (random-integer n)))
          (unless (hash-get used idx)
            (hash-put! used idx #t)
            (vector-set! chosen picked-count (vector-copy (vector-ref point-vec idx)))
            (set! picked-count (+ picked-count 1))))
        (loop)))
    chosen))
```

`vector-copy` is important here because the algorithm mutates the centroid vectors during the update step, and we do not want those mutations to accidentally corrupt the original data points.

### K-means++ Initialization

The k-means++ scheme picks the first centroid uniformly, then for each subsequent centroid computes the squared distance from every point to its nearest already-chosen centroid, treats those distances as an unnormalized probability distribution, and samples one point from it:

```scheme
;; K-means++ initialization: pick the first centroid uniformly, then each
;; subsequent centroid with probability proportional to its squared distance
;; from the nearest already-chosen centroid.
(def (initialize-centroids-plusplus points k)
  (let* ((point-vec (list->vector points))
         (n (vector-length point-vec))
         (centroids (make-vector k #f)))
    (vector-set! centroids 0
                 (vector-copy (vector-ref point-vec (random-integer n))))
    (do ((c 1 (+ c 1)))
        ((= c k) centroids)
      (let ((distances (make-vector n 0.0))
            (total 0.0))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (let ((best 1.0e30))
            (do ((j 0 (+ j 1)))
                ((= j c))
              (let ((d (squared-distance (vector-ref point-vec i)
                                         (vector-ref centroids j))))
                (when (< d best) (set! best d))))
            (vector-set! distances i best)
            (set! total (+ total best))))
        (if (= total 0.0)
          (vector-set! centroids c
                       (vector-copy (vector-ref point-vec (random-integer n))))
          (let ((threshold (* (random-real) total))
                (running 0.0)
                (picked #f))
            (do ((i 0 (+ i 1)))
                ((or picked (= i n)))
              (set! running (+ running (vector-ref distances i)))
              (when (>= running threshold)
                (vector-set! centroids c
                             (vector-copy (vector-ref point-vec i)))
                (set! picked #t)))
            (unless picked
              (vector-set! centroids c
                           (vector-copy (vector-ref point-vec (- n 1)))))))))))
```

The cumulative-sum sampling trick avoids explicitly building a normalized probability vector: draw a uniform threshold from `[0, \text{total}]`$, walk the distances until the running sum exceeds the threshold, and pick that point. Points far from the existing centroids contribute more mass and are more likely to be picked, which spreads the initial centroids out.

### A Single K-means Run

`kmeans-single-run` performs one execution of the assign / update loop for a given initialization. It stops as soon as the centroids stop moving or the iteration cap is reached, and returns the centroids, labels, inertia, and iteration count for that run:

```scheme
;; Run a single k-means pass with the given initial centroids.
;; Returns (centroids labels inertia iterations).
(def (kmeans-single-run points k num-features max-iters tol init-centroids)
  (let ((centroids init-centroids)
        (labels (make-vector (length points) 0))
        (iter 0)
        (done #f))
    (let loop ()
      (unless (or done (>= iter max-iters))
        (set! iter (+ iter 1))
        (set! labels (assign-clusters points centroids))
        (let ((new-centroids
               (update-centroids points labels k num-features centroids)))
          (when (centroids-converged? centroids new-centroids tol)
            (set! done #t))
          (set! centroids new-centroids))
        (loop)))
    (list centroids labels (compute-inertia points labels centroids) iter)))
```

### The Public Fit Function

`kmeans-fit` is the top-level entry point. It handles keyword arguments, runs `n-init` restarts, keeps the best one by inertia, and packages the result in a hash table:

```scheme
;; Fit a k-means model.
;;   points        : list of feature vectors (all same length)
;;   k             : number of clusters
;;   max-iters:    : max iterations per run (default 100)
;;   tol:          : convergence tolerance on centroid movement (default 1e-6)
;;   n-init:       : number of random restarts; best-inertia run wins (default 10)
;;   init:         : 'kmeans++ (default) or 'random
;; Returns a hash table with keys: centroids, labels, inertia, iterations, k, num-features.
(def (kmeans-fit points k
                 max-iters: (max-iters 100)
                 tol: (tol 1.0e-6)
                 n-init: (n-init 10)
                 init: (init 'kmeans++))
  (when (null? points)
    (error "kmeans-fit: empty point list"))
  (let* ((num-features (vector-length (car points)))
         (best-inertia 1.0e30)
         (best-centroids #f)
         (best-labels #f)
         (best-iters 0))
    (do ((run 0 (+ run 1)))
        ((= run n-init))
      (let* ((init-c (case init
                       ((random) (initialize-centroids-random points k))
                       (else     (initialize-centroids-plusplus points k))))
             (result (kmeans-single-run points k num-features
                                        max-iters tol init-c))
             (cents (car result))
             (labs (cadr result))
             (inert (caddr result))
             (its (cadddr result)))
        (when (< inert best-inertia)
          (set! best-inertia inert)
          (set! best-centroids cents)
          (set! best-labels labs)
          (set! best-iters its))))
    (let ((model (make-hash-table)))
      (hash-put! model 'centroids best-centroids)
      (hash-put! model 'labels best-labels)
      (hash-put! model 'inertia best-inertia)
      (hash-put! model 'iterations best-iters)
      (hash-put! model 'k k)
      (hash-put! model 'num-features num-features)
      model)))
```

Keyword arguments follow Gerbil's `key: (name default)` convention. Notice how `case` dispatches on the `init` symbol so that `'random` selects the uniform initializer and any other value (including `'kmeans++`) selects the k-means++ initializer.

### Predict and Accessors

The remaining public API is small. `kmeans-predict` returns the cluster index for a new point, and four one-line accessors pull individual fields out of the trained model hash table:

```scheme
;; Predict the cluster label for a single point vector.
(def (kmeans-predict model x)
  (nearest-centroid-index x (hash-ref model 'centroids)))

;; Convenience accessors
(def (centroids-of model)  (hash-ref model 'centroids))
(def (labels-of model)     (hash-ref model 'labels))
(def (inertia-of model)    (hash-ref model 'inertia))
(def (iterations-of model) (hash-ref model 'iterations))
```

## The Demo Program: test.ss

The demo generates the three Gaussian blobs described earlier, fits the model, prints diagnostics, and classifies a few probe points. It also runs a second fit with random initialization to prove the option is wired up.

### Generating Synthetic Data

Gerbil ships with a uniform `random-real` primitive but not a Gaussian one, so we implement the Box-Muller transform in a few lines. Given two independent uniform samples `u_1, u_2 \in (0, 1]`$, the transform returns a sample from `N(0, 1)`$:

```$
z = \sqrt{-2 \ln u_1} \cdot \cos(2 \pi u_2)
```

The `make-blob` procedure calls `random-normal` twice per point to produce a 2-D sample centered at `(cx, cy)` with the requested standard deviation:

```scheme
;; Box-Muller transform: draw a single sample from N(0, 1).
(def (random-normal)
  (let ((u1 (max 1.0e-10 (random-real)))
        (u2 (random-real)))
    (* (sqrt (* -2.0 (log u1)))
       (cos (* 2.0 (acos -1.0) u2)))))

;; Generate `n` points around (cx, cy) with the given standard deviation.
(def (make-blob n cx cy std)
  (let loop ((i 0) (acc '()))
    (if (= i n)
      acc
      (loop (+ i 1)
            (cons (vector (+ cx (* std (random-normal)))
                          (+ cy (* std (random-normal))))
                  acc)))))
```

The `max 1.0e-10` clamp on `u_1`$ prevents the extremely unlikely case where `random-real` returns exactly `0.0`$, which would make `log u_1`$ undefined.

### Reporting Helpers

Three small helpers count cluster sizes, sum a vector, and pretty-print the centroids and cluster sizes:

```scheme
;; Count how many points fall in each cluster.
(def (cluster-sizes labels k)
  (let ((counts (make-vector k 0))
        (n (vector-length labels)))
    (do ((i 0 (+ i 1)))
        ((= i n) counts)
      (let ((c (vector-ref labels i)))
        (vector-set! counts c (+ (vector-ref counts c) 1))))))

;; Given cluster sizes, return their sum (must equal total point count).
(def (sum-vec v)
  (let ((n (vector-length v))
        (s 0))
    (do ((i 0 (+ i 1)))
        ((= i n) s)
      (set! s (+ s (vector-ref v i))))))

(def (print-centroids centroids)
  (let ((k (vector-length centroids)))
    (do ((c 0 (+ c 1)))
        ((= c k))
      (let ((v (vector-ref centroids c)))
        (displayln
          (format "  centroid ~a: (~a, ~a)"
                  c
                  (format-decimal (vector-ref v 0) 3)
                  (format-decimal (vector-ref v 1) 3)))))))

(def (print-cluster-sizes counts)
  (let ((k (vector-length counts)))
    (do ((c 0 (+ c 1)))
        ((= c k))
      (displayln (format "  cluster ~a: ~a points"
                         c (vector-ref counts c))))))
```

### The Main Entry Point

The `main` procedure ties everything together: generate the blobs, fit, print centroids and cluster sizes, run a few probe predictions, then repeat the fit with random init to compare inertias:

```scheme
(def (main)
  (random-source-randomize! default-random-source)
  (displayln "=== K-means clustering demo ===\n")

  ;; Three well-separated blobs.
  (let* ((blob-a (make-blob 60  0.0  0.0 0.35))
         (blob-b (make-blob 60  4.0  0.0 0.35))
         (blob-c (make-blob 60  2.0  3.5 0.35))
         (points (append blob-a blob-b blob-c))
         (k 3))
    (displayln (format "Generated ~a points across 3 true blobs." (length points)))
    (displayln (format "True centers: (0,0), (4,0), (2,3.5)\n"))

    ;; Fit with k-means++ initialization.
    (let* ((model (kmeans-fit points k n-init: 10 max-iters: 100)))
      (displayln (format "Converged in ~a iterations (best of 10 restarts)."
                         (iterations-of model)))
      (displayln (format "Final inertia (WCSS): ~a"
                         (format-decimal (inertia-of model) 4)))
      (displayln "Learned centroids:")
      (print-centroids (centroids-of model))
      (displayln "Cluster sizes:")
      (print-cluster-sizes (cluster-sizes (labels-of model) k))

      ;; Sanity checks.
      (unless (= (sum-vec (cluster-sizes (labels-of model) k)) (length points))
        (error "cluster sizes must sum to point count"))
      (unless (= (vector-length (centroids-of model)) k)
        (error "expected k centroids"))
      (unless (< (inertia-of model) 100.0)
        (error "inertia unexpectedly large; blobs should cluster tightly"))

      ;; Predict a few fresh points near each true center.
      (displayln "\nPredictions for new points:")
      (for-each
        (lambda (probe)
          (let* ((label (kmeans-predict model probe)))
            (displayln
              (format "  point (~a, ~a) -> cluster ~a"
                      (format-decimal (vector-ref probe 0) 2)
                      (format-decimal (vector-ref probe 1) 2)
                      label))))
        (list (vector 0.1  0.1)
              (vector 3.9  0.1)
              (vector 2.0  3.4)
              (vector 2.0  1.5))))

    ;; Compare against random init to demonstrate the option is wired up.
    (displayln "\n--- Sanity check: random init also converges ---")
    (let ((model2 (kmeans-fit points k n-init: 10 init: 'random)))
      (displayln (format "random-init inertia: ~a  (iters: ~a)"
                         (format-decimal (inertia-of model2) 4)
                         (iterations-of model2))))

    (displayln "\n=== Demo complete ===")))

(main)
```

The final probe `(2.0, 1.5)`$ is deliberately near the geometric center of the three blobs. It should end up assigned to whichever cluster happens to be nearest, which depends on the random seed but is always a defensible choice.

## Running the Demo

The Makefile is minimal:

```makefile
test:
	gxi test.ss
```

Run either `make test` or invoke the interpreter directly with `gxi test.ss`. A typical run produces output similar to the following. Because the blob generation and initialization both use randomness, the exact numbers will vary from run to run, and the cluster indices (0, 1, 2) may be permuted:

```console
$ make test
gxi test.ss
=== K-means clustering demo ===

Generated 180 points across 3 true blobs.
True centers: (0,0), (4,0), (2,3.5)

Converged in 2 iterations (best of 10 restarts).
Final inertia (WCSS): 39.1594
Learned centroids:
  centroid 0: (1.983, 3.497)
  centroid 1: (4.056, 0.010)
  centroid 2: (-0.012, 0.069)
Cluster sizes:
  cluster 0: 60 points
  cluster 1: 60 points
  cluster 2: 60 points

Predictions for new points:
  point (0.10, 0.10) -> cluster 2
  point (3.90, 0.10) -> cluster 1
  point (2.00, 3.40) -> cluster 0
  point (2.00, 1.50) -> cluster 0

--- Sanity check: random init also converges ---
random-init inertia: 39.1594  (iters: 3)

=== Demo complete ===
```

## Interpreting the Results

The output reports several quantities that are worth understanding in the context of the algorithm.

- **Converged in 2 iterations**: with k-means++ initialization and well-separated blobs, the initial centroids are already very close to the true means. Two passes are enough to move them into place and detect that they have stopped moving. Poorly initialized runs, or runs on noisier data, would need many more.
- **Final inertia = 39.1594**: this is the total squared distance from every point to its assigned centroid. Recall that each blob has standard deviation `0.35`$ in two dimensions. The expected inertia per point is roughly `2 \cdot \sigma^2 = 2 \cdot 0.35^2 = 0.245`$, so for 180 points we expect an inertia around `44.1`$. The measured value `39.1594`$ is right in that ballpark, confirming the centroids are essentially at the true blob means.
- **Learned centroids**: `(1.983, 3.497)`$, `(4.056, 0.010)`$, and `(-0.012, 0.069)`$. Compare these to the true centers `(2.0, 3.5)`$, `(4.0, 0.0)`$, and `(0.0, 0.0)`$. Each learned coordinate is within about `0.06`$ of the truth, which is well within the noise level of `\sigma = 0.35`$ divided by `\sqrt{60} \approx 0.045`$ (the standard error of the mean of 60 samples).
- **Cluster sizes**: all three clusters contain exactly 60 points, meaning K-means separated the blobs perfectly. Not a single point crossed a decision boundary.
- **Predictions for new points**: the probe points near `(0, 0)`$, `(4, 0)`$, and `(2, 3.4)`$ are placed in the corresponding true clusters. The ambiguous probe at `(2.0, 1.5)`$ is assigned to whichever cluster centroid it happens to be closest to. In the run above that was cluster 0 (the top blob at `(1.983, 3.497)`$), which was closer than the two bottom-row centroids.
- **Random-init inertia identical to k-means++ inertia**: on such a clean dataset, both initializers find the same global minimum. On messier data, k-means++ typically finds a lower inertia than uniform random init, especially at low `n-init`$.

The identical inertia between the two fitting runs is a useful check that both initialization codepaths are exercising the same core algorithm rather than one being subtly broken.

## Wrap Up

We built a general-purpose K-means clustering library in Gerbil Scheme in around 200 lines of code. The library covers the essentials: k-means++ and uniform initialization, an assign-update main loop with tolerance-based convergence, empty-cluster handling, multiple random restarts scored by inertia, and both `fit` and `predict` operations.

A few observations transfer directly to any dataset you might want to cluster:

- Vectors of fixed length are the right representation for feature points in Gerbil. Constant-time indexing keeps the tight inner loops (distance and mean computation) fast.
- Squared distance is enough for K-means. Only compute the square root when reporting to a user.
- Multiple random restarts (`n-init`$ of `10`$ is a good default) plus inertia-based selection is the simplest general defense against poor local minima.
- Wrap the trained model in a hash table with named keys. That gives you a stable API surface (`centroids-of`, `labels-of`, ...) even as internal representations evolve.

The one big caveat with K-means is that you have to pick `k`$ up front. On real-world data you rarely know the true number of clusters. The most common heuristic is the "elbow method": run `kmeans-fit` for a range of `k`$ values, plot the inertia as a function of `k`$, and pick the `k`$ at which the curve bends from steep to shallow. That is a natural extension exercise for readers, along with several other useful additions covered in the practice problems below.

## Optional Practice Problems

1. **Elbow-method plot data**: Write a procedure `(inertia-curve points k-min k-max)` that fits K-means for each `k`$ from `k-min`$ to `k-max`$ and returns a list of `(k . inertia)`$ pairs. Print the pairs and identify the `k`$ where the drop in inertia flattens. Verify that for the three-blob dataset the elbow is at `k = 3`$.

2. **Silhouette score**: The silhouette score is a standard cluster-quality metric that does not require ground truth. For a point `x_i`$ in cluster `A`$ with mean intra-cluster distance `a(i)`$ and mean distance `b(i)`$ to points in the nearest other cluster, the silhouette is `s(i) = (b(i) - a(i)) / \max(a(i), b(i))`$. Implement `(silhouette-score points labels)` and report the average silhouette on the three-blob dataset. A well-clustered dataset should score close to `1.0`$.

3. **Mini-batch K-means**: The full K-means pass touches every point on every iteration, which becomes expensive on large datasets. Implement `(kmeans-fit-minibatch points k batch-size max-iters)` that on each iteration samples `batch-size`$ points at random, assigns them to their nearest centroid, and moves each centroid by a small step toward the mean of its assigned batch points. Compare inertia and runtime to the full-batch fit.

4. **Save and load a trained model**: Implement `(save-kmeans model path)` and `(load-kmeans path)` that serialize the trained centroids, `k`$, and `num-features`$ to a plain-text file and read them back later. Verify that `(kmeans-predict (load-kmeans path) x)` returns the same label as the in-memory model for the same input `x`$.

5. **Cluster real data**: The classic Iris dataset has 150 points in 4-D space and 3 known species labels. Download `iris.data`, load it into a list of feature vectors (dropping the species label into a separate list), fit `kmeans-fit` with `k = 3`$, and report how well the discovered clusters align with the true species using a confusion matrix. Because K-means assigns cluster indices arbitrarily, you will need to find the best mapping between cluster indices and true labels before scoring.
