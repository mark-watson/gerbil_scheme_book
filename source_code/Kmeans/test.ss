;; File: test.ss
;; Demo / test for the K-means library.
;; Generates three 2-D Gaussian blobs and clusters them.
(import "kmeans-lib")
(import :std/format
        :gerbil/gambit)

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
