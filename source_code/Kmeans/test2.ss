;; test2.ss - gxi compatible, no module resolution
;; run with: gxi test2.ss
(import :std/format
        :gerbil/gambit)

;; load the library textually - avoids  ./kmeans-lib  module lookup
(include "kmeans-lib.ss")

(def failures 0)
(def total 0)

(def (report ok? msg)
  (set! total (+ total 1))
  (if ok?
    (begin (displayln "  PASS: " msg))
    (begin (set! failures (+ failures 1))
           (displayln "  FAIL: " msg))))

(def (check-equal msg got expected)
  (report (equal? got expected)
          (format "~a => got ~a want ~a" msg got expected)))

(def (check-true msg v)
  (report (if v #t #f) msg))

(def (approx=? a b (eps 1e-6))
  (< (abs (- a b)) eps))

(def (vec-approx=? a b (eps 1e-6))
  (and (= (vector-length a) (vector-length b))
       (let loop ((i 0))
         (or (= i (vector-length a))
             (and (approx=? (vector-ref a i) (vector-ref b i) eps)
                  (loop (+ i 1)))))))

(displayln "=== format-decimal ===")
(check-equal "3.14159 2" (format-decimal 3.14159 2) "3.14")
(check-equal "3.1 2" (format-decimal 3.1 2) "3.10")
(check-equal "3 2" (format-decimal 3 2) "3.00")
(check-equal "2.999 2" (format-decimal 2.999 2) "3.00")

(displayln "=== distances ===")
(check-true "squared 25" (approx=? (squared-distance #(0 0) #(3 4)) 25.0))
(check-true "euclid 5" (approx=? (euclidean-distance #(0 0) #(3 4)) 5.0))

(displayln "=== assign-clusters ===")
(let* ((cents (vector #(0. 0.) #(10. 10.)))
       (pts (list #(0.1 0.1) #(9.9 10.1)))
       (labs (assign-clusters pts cents)))
  (check-equal "label 0" (vector-ref labs 0) 0)
  (check-equal "label 1" (vector-ref labs 1) 1))

(displayln "=== update-centroids empty cluster ===")
(let* ((pts (list #(1. 1.) #(2. 2.)))
       (labs (vector 0 0))
       (prev (vector #(0. 0.) #(99. 99.)))
       (new (update-centroids pts labs 2 2 prev)))
  (check-true "keep prev" (vec-approx=? (vector-ref new 1) #(99. 99.))))

(displayln "=== compute-inertia ===")
(check-true "inertia 0.5"
  (approx=? (compute-inertia (list #(0. 0.) #(1. 0.)) (vector 0 0) (vector #(0.5 0.))) 0.5))

(displayln "=== kmeans-fit ===")
(let* ((pts (list #(0. 0.) #(0. 1.) #(1. 0.)
                  #(10. 10.) #(10. 11.) #(11. 10.)))
       (m (kmeans-fit pts 2 n-init: 3 init: 'kmeans++)))
  (check-equal "k" (hash-ref m 'k) 2)
  (check-equal "centroids len" (vector-length (centroids-of m)) 2)
  (check-true "inertia >0" (> (inertia-of m) 0.))
  (let ((p0 (kmeans-predict m #(0.1 0.1)))
        (p1 (kmeans-predict m #(10. 10.))))
    (check-true "predict separates" (not (= p0 p1)))))

(displayln (format "--- ~a checks, ~a failures" total failures))
(exit (if (zero? failures) 0 1))
