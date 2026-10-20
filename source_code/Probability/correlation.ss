;; File: correlation.ss
;; Correlation helpers.
;;
;; NOTE: Correlation measures *association*, NOT causation.
;; A high |r| between X and Y does not imply that X causes Y,
;; that Y causes X, or even that they share a direct mechanism.
;; Confounders, selection bias, and reverse causation are always possible.

(import :std/sort)
(export mean std-dev rank-list pearson-r spearman-rho correlation-matrix)

;; Sum a list of numbers (fold avoids apply's argument-count limit on big lists).
(def (sum xs) (foldl + 0 xs))

;; Arithmetic mean of a list of numbers.
(def (mean xs)
  (/ (sum xs) (length xs)))

;; Population standard deviation of a list of numbers.
(def (std-dev xs)
  (let ((m (mean xs)))
    (sqrt (/ (sum (map (lambda (x) (expt (- x m) 2)) xs))
             (length xs)))))

;; Return a list of ranks (1-based, average ties) for XS.
(def (rank-list xs)
  (let* ((n (length xs))
         (indexed (let loop ((l xs) (i 0) (acc '()))
                    (if (null? l)
                      (reverse acc)
                      (loop (cdr l) (+ i 1) (cons (cons (car l) i) acc)))))
         (sorted (list->vector (sort indexed (lambda (a b) (< (car a) (car b))))))
         (ranks (make-vector n 0.0)))
    ;; Walk the sorted vector, assigning the average rank to each tie group.
    (let loop ((i 0))
      (when (< i n)
        (let find-end ((j i))
          (if (and (< j n)
                   (= (car (vector-ref sorted j)) (car (vector-ref sorted i))))
            (find-end (+ j 1))
            ;; average of ranks (i+1)..j is (i+1+j)/2
            (let ((avg (/ (+ i 1 j) 2.0)))
              (do ((k i (+ k 1))) ((= k j))
                (vector-set! ranks (cdr (vector-ref sorted k)) avg))
              (loop j))))))
    (vector->list ranks)))

;; Pearson product-moment correlation coefficient.
;; WARNING: measures linear association only -- not causation.
(def (pearson-r xs ys)
  (unless (= (length xs) (length ys))
    (error "XS and YS must have the same length."))
  (let ((n (length xs))
        (mx (mean xs))
        (my (mean ys))
        (sx (std-dev xs))
        (sy (std-dev ys)))
    (if (or (zero? sx) (zero? sy))
      0.0  ; a constant variable has no correlation
      (/ (sum (map (lambda (x y) (* (- x mx) (- y my))) xs ys))
         (* n sx sy)))))

;; Spearman rank-order correlation: Pearson r computed on ranks.
;; WARNING: measures monotonic association only -- not causation.
(def (spearman-rho xs ys)
  (pearson-r (rank-list xs) (rank-list ys)))

;; Given DATA-ALIST of (NAME . VALUES-LIST) pairs, return a list of
;; (NAME-A NAME-B . PEARSON-R) triples for all pairs.
(def (correlation-matrix data-alist)
  (append-map
   (lambda (a)
     (map (lambda (b)
            (cons (car a) (cons (car b) (pearson-r (cdr a) (cdr b)))))
          data-alist))
   data-alist))

(def (append-map f l) (apply append (map f l)))
