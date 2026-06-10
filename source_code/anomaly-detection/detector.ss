;; File: detector.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

(export split-data
        compute-mu
        compute-sigma-sq
        gaussian-p
        build-detector
        train
        is-anomaly
        test-model
        format-decimal)

(def SQRT_2_PI (sqrt (* 2.0 (acos -1.0))))

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
         (str (number->string rounded)))
    (let ((dot-pos (string-index str #\.)))
      (let ((formatted-str
             (if dot-pos
               (let* ((frac-len (- (string-length str) dot-pos 1))
                      (diff (- places frac-len)))
                 (if (> diff 0)
                   (string-append str (make-string diff #\0))
                   str))
               (string-append str "." (make-string places #\0)))))
        (if (char=? (string-ref formatted-str 0) #\.)
          (string-append "0" formatted-str)
          formatted-str)))))

;; Split examples into training (~60%), cross-validation (~20%), and test sets (~20%)
(def (split-data examples num-features)
  (let ((train '())
        (cv '())
        (test '())
        (oi (- num-features 1)))
    (for-each
      (lambda (ex)
        (if (< (random-real) 0.6)
          (when (or (< (vector-ref ex oi) 0.5) (< (random-real) 0.1))
            (set! train (cons ex train)))
          (if (< (random-real) 0.7)
            (set! cv (cons ex cv))
            (set! test (cons ex test)))))
      examples)
    `((train . ,(reverse train))
      (cv . ,(reverse cv))
      (test . ,(reverse test)))))

;; Per-feature mean mu
(def (compute-mu examples nf)
  (let* ((mu (make-vector nf 0.0))
         (len (length examples)))
    (if (= len 0)
      mu
      (begin
        (for-each
          (lambda (ex)
            (do ((f 0 (+ f 1)))
                ((= f nf))
              (vector-set! mu f (+ (vector-ref mu f) (vector-ref ex f)))))
          examples)
        (do ((f 0 (+ f 1)))
            ((= f nf) mu)
          (vector-set! mu f (/ (vector-ref mu f) len)))))))

;; Per-feature variance sigmaSq
(def (compute-sigma-sq examples mu nf)
  (let* ((s2 (make-vector nf 0.0))
         (len (length examples)))
    (do ((f 0 (+ f 1)))
        ((= f (- nf 1)) s2)
      (let ((sum-val 0.0))
        (for-each
          (lambda (ex)
            (let* ((d (- (vector-ref ex f) (vector-ref mu f))))
              (set! sum-val (+ sum-val (* d d)))))
          examples)
        (vector-set! s2 f (max (/ sum-val len) 1.0e-10))))))

;; Average Gaussian PDF p(x) across all features
(def (gaussian-p x mu sigma-sq nf)
  (let ((sum-val 0.0))
    (do ((f 0 (+ f 1)))
        ((= f (- nf 1)) (/ sum-val nf))
      (let* ((s2 (vector-ref sigma-sq f))
             (d (- (vector-ref x f) (vector-ref mu f)))
             (pdf (* (/ 1.0 (* SQRT_2_PI (sqrt s2))) (exp (- (/ (* d d) (* 2.0 s2)))))))
        (set! sum-val (+ sum-val pdf))))))

;; Build a detector from labelled examples
(def (build-detector examples num-features)
  (let* ((split (split-data examples num-features))
         (train (cdr (assoc 'train split)))
         (cv (cdr (assoc 'cv split)))
         (test (cdr (assoc 'test split)))
         (mu (compute-mu train num-features))
         (sigma-sq (make-vector num-features 0.0))
         (ht (make-hash-table)))
    (hash-put! ht 'num-features num-features)
    (hash-put! ht 'mu mu)
    (hash-put! ht 'sigma-sq sigma-sq)
    (hash-put! ht 'best-eps 0.02)
    (hash-put! ht 'training train)
    (hash-put! ht 'cross-validation cv)
    (hash-put! ht 'testing test)
    ht))

(def (train-helper det epsilon)
  (let* ((nf (hash-ref det 'num-features))
         (training (hash-ref det 'training))
         (mu (hash-ref det 'mu))
         (cv (hash-ref det 'cross-validation))
         (sigma-sq (compute-sigma-sq training mu nf))
         (errors 0))
    (hash-put! det 'sigma-sq sigma-sq)
    (for-each
      (lambda (x)
        (let* ((prob (gaussian-p x mu sigma-sq nf))
               (anomaly (> (vector-ref x (- nf 1)) 0.5)))
          (if (if anomaly (> prob epsilon) (< prob epsilon))
            (set! errors (+ errors 1))
            #f)))
      cv)
    errors))

;; Evaluate model on test data
(def (test-model det epsilon)
  (let* ((nf (hash-ref det 'num-features))
         (mu (hash-ref det 'mu))
         (sigma-sq (hash-ref det 'sigma-sq))
         (testing (hash-ref det 'testing))
         (tp 0) (fp 0) (fn 0) (tn 0))
    (for-each
      (lambda (x)
        (let* ((prob (gaussian-p x mu sigma-sq nf))
               (anomaly (> (vector-ref x (- nf 1)) 0.5)))
          (if anomaly
            (if (> prob epsilon) (set! fn (+ fn 1)) (set! tp (+ tp 1)))
            (if (< prob epsilon) (set! fp (+ fp 1)) (set! tn (+ tn 1))))))
      testing)
    (let* ((precision (if (= (+ tp fp) 0) 0.0 (/ (exact->inexact tp) (+ tp fp))))
           (recall (if (= (+ tp fn) 0) 0.0 (/ (exact->inexact tp) (+ tp fn))))
           (f1 (if (= (+ precision recall) 0.0) 0.0 (/ (* 2.0 precision recall) (+ precision recall)))))
      (displayln (format "\n -- best epsilon = ~a" (format-decimal epsilon 4)))
      (displayln (format " -- test examples  = ~a" (length testing)))
      (displayln (format " -- TP=~a FP=~a FN=~a TN=~a" tp fp fn tn))
      (displayln (format " -- precision=~a recall=~a F1=~a"
                         (format-decimal precision 4)
                         (format-decimal recall 4)
                         (format-decimal f1 4)))
      `((precision . ,precision)
        (recall . ,recall)
        (f1 . ,f1)))))

;; Sweep 200 epsilon values, pick best, evaluate on test set
(def (train det)
  (let ((best-err 1.0e10)
        (best-e 0.001))
    (do ((i 0 (+ i 1)))
        ((= i 200))
      (let* ((eps (+ 0.001 (* 0.005 i)))
             (err (train-helper det eps)))
        (when (<= err best-err)
          (set! best-err err)
          (set! best-e eps))))
    (displayln (format "\n**** Best epsilon = ~a" (format-decimal best-e 4)))
    (hash-put! det 'best-eps best-e)
    (train-helper det best-e)
    (test-model det best-e)
    det))

;; Return true if x is classified as an anomaly
(def (is-anomaly det x)
  (< (gaussian-p x (hash-ref det 'mu) (hash-ref det 'sigma-sq) (hash-ref det 'num-features))
     (hash-ref det 'best-eps)))
