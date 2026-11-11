;; File: kmeans-lib.ss
;; K-means clustering library
(import :std/format
        :gerbil/gambit
        "../shared/format")

(export euclidean-distance
        squared-distance
        assign-clusters
        update-centroids
        compute-inertia
        initialize-centroids-random
        initialize-centroids-plusplus
        kmeans-fit
        kmeans-predict
        kmeans-model
        kmeans-model?
        kmeans-model-centroids
        kmeans-model-labels
        kmeans-model-inertia
        kmeans-model-iterations
        kmeans-model-k
        kmeans-model-num-features
        centroids-of
        labels-of
        inertia-of
        iterations-of
        format-decimal)

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

;; Uniform random selection of k distinct starting points.
(def (initialize-centroids-random points k)
  (let* ((point-vec (list->vector points))
         (n (vector-length point-vec)))
    (when (> k n)
      (error "initialize-centroids-random: k cannot exceed number of points" k n))
    (let ((chosen (make-vector k #f))
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
      chosen)))

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
    ;; Reassign labels once for final centroids so labels and inertia are strictly consistent:
    (let ((final-labels (assign-clusters points centroids)))
      (list centroids final-labels (compute-inertia points final-labels centroids) iter))))

(defstruct kmeans-model
  (centroids labels inertia iterations k num-features)
  transparent: #t)

;; Fit a k-means model.
;;   points        : list of feature vectors (all same length)
;;   k             : number of clusters
;;   max-iters:    : max iterations per run (default 100)
;;   tol:          : convergence tolerance on centroid movement (default 1e-6)
;;   n-init:       : number of random restarts; best-inertia run wins (default 10)
;;   init:         : 'kmeans++ (default) or 'random
;; Returns a kmeans-model struct.
(def (kmeans-fit points k
                 max-iters: (max-iters 100)
                 tol: (tol 1.0e-6)
                 n-init: (n-init 10)
                 init: (init 'kmeans++))
  (when (null? points)
    (error "kmeans-fit: empty point list"))
  (when (<= k 0)
    (error "kmeans-fit: k must be positive" k))
  (when (> k (length points))
    (error "kmeans-fit: k cannot exceed number of points" k (length points)))
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
    (make-kmeans-model best-centroids best-labels best-inertia best-iters k num-features)))

;; Predict the cluster label for a single point vector.
(def (kmeans-predict model x)
  (nearest-centroid-index x (centroids-of model)))

;; Convenience accessors (work with kmeans-model struct or hash table)
(def (centroids-of model)
  (if (kmeans-model? model) (kmeans-model-centroids model) (hash-ref model 'centroids)))
(def (labels-of model)
  (if (kmeans-model? model) (kmeans-model-labels model) (hash-ref model 'labels)))
(def (inertia-of model)
  (if (kmeans-model? model) (kmeans-model-inertia model) (hash-ref model 'inertia)))
(def (iterations-of model)
  (if (kmeans-model? model) (kmeans-model-iterations model) (hash-ref model 'iterations)))
