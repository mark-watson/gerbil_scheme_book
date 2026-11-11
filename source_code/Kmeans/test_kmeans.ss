;; File: test_kmeans.ss
(import :std/test
        :std/format
        "kmeans-lib")

(export main)

(def (approx=? a b (eps 1e-6))
  (< (abs (- a b)) eps))

(def (vec-approx=? a b (eps 1e-6))
  (and (= (vector-length a) (vector-length b))
       (let loop ((i 0))
         (or (= i (vector-length a))
             (and (approx=? (vector-ref a i) (vector-ref b i) eps)
                  (loop (+ i 1)))))))

(def kmeans-suite
  (test-suite "K-means test suite"
    (test-case "distances"
      (check (approx=? (squared-distance #(0 0) #(3 4)) 25.0) => #t)
      (check (approx=? (euclidean-distance #(0 0) #(3 4)) 5.0) => #t))

    (test-case "assign-clusters"
      (let* ((cents (vector #(0. 0.) #(10. 10.)))
             (pts (list #(0.1 0.1) #(9.9 10.1)))
             (labs (assign-clusters pts cents)))
        (check (vector-ref labs 0) => 0)
        (check (vector-ref labs 1) => 1)))

    (test-case "update-centroids empty cluster"
      (let* ((pts (list #(1. 1.) #(2. 2.)))
             (labs (vector 0 0))
             (prev (vector #(0. 0.) #(99. 99.)))
             (new (update-centroids pts labs 2 2 prev)))
        (check (vec-approx=? (vector-ref new 1) #(99. 99.)) => #t)))

    (test-case "compute-inertia"
      (check (approx=? (compute-inertia (list #(0. 0.) #(1. 0.))
                                        (vector 0 0)
                                        (vector #(0.5 0.)))
                       0.5) => #t))

    (test-case "kmeans-fit with kmeans-model struct"
      (let* ((pts (list #(0. 0.) #(0. 1.) #(1. 0.)
                        #(10. 10.) #(10. 11.) #(11. 10.)))
             (m (kmeans-fit pts 2 n-init: 3 init: 'kmeans++)))
        (check (kmeans-model? m) => #t)
        (check (kmeans-model-k m) => 2)
        (check (vector-length (centroids-of m)) => 2)
        (check (> (inertia-of m) 0.0) => #t)
        (let ((p0 (kmeans-predict m #(0.1 0.1)))
              (p1 (kmeans-predict m #(10. 10.))))
          (check (not (= p0 p1)) => #t))))))

(def (main . args)
  (run-tests! kmeans-suite)
  (test-report-summary!)
  (exit (case (test-result) ((OK) 0) (else 1))))
