;; File: wisconsin_demo.ss
(import "detector")
(import :std/misc/ports
        :std/pregexp
        :std/format
        :gerbil/gambit)

;; Trim leading and trailing whitespace
(def (string-trim str)
  (pregexp-replace "^\\s+" (pregexp-replace "\\s+$" str "") ""))

(def (load-csv path)
  (let ((lines (read-file-lines path)))
    (let loop ((lines lines) (acc '()))
      (if (null? lines)
        (reverse acc)
        (let* ((line (string-trim (car lines)))
               (trimmed-len (string-length line)))
          (if (> trimmed-len 0)
            (let* ((tokens (pregexp-split "," line))
                   (numbers (map string->number tokens))
                   (vec (list->vector numbers)))
              (loop (cdr lines) (cons vec acc)))
            (loop (cdr lines) acc)))))))

;; Log-transform, per-row min-max scaling, remap target {2,4} -> {0,1}
(def (preprocess-wisconsin rows)
  (map
    (lambda (row)
      (let* ((xs (make-vector 10 0.0))
             (mn 1.0e9)
             (mx -1.0e9))
        ;; Multiply feature values (indices 0 to 8) by 0.1
        (do ((i 0 (+ i 1)))
            ((= i 9))
          (vector-set! xs i (* (vector-ref row i) 0.1)))
        ;; Copy target (index 9)
        (vector-set! xs 9 (vector-ref row 9))
        
        ;; Compute log and find min/max
        (do ((i 0 (+ i 1)))
            ((= i 9))
          (let ((v (log (+ (vector-ref xs i) 1.2))))
            (vector-set! xs i v)
            (when (< v mn) (set! mn v))
            (when (> v mx) (set! mx v))))
        
        ;; Min-max scale the features
        (let ((range (- mx mn)))
          (do ((i 0 (+ i 1)))
              ((= i 9))
            (if (< range 1.0e-10)
              (vector-set! xs i 0.5)
              (vector-set! xs i (/ (- (vector-ref xs i) mn) range)))))
        
        ;; Normalize target from {2.0, 4.0} to {0.0, 1.0}
        (vector-set! xs 9 (* (- (vector-ref xs 9) 2.0) 0.5))
        xs))
    rows))

(def (main)
  (let* ((data (preprocess-wisconsin (load-csv "data/cleaned_wisconsin_cancer_data.csv"))))
    (displayln (format "Loaded ~a examples." (length data)))
    
    (let* ((det (build-detector data 10)))
      (displayln (format "\nTraining set:  ~a" (length (hash-ref det 'training))))
      (displayln (format "Cross-val set: ~a" (length (hash-ref det 'cross-validation))))
      (displayln (format "Test set:      ~a" (length (hash-ref det 'testing))))
      
      (train det)
      
      (displayln (format "\nModel: bestEps=~a, features=~a"
                         (format-decimal (hash-ref det 'best-eps) 4)
                         (hash-ref det 'num-features)))
      
      ;; Assertions
      (unless (> (hash-ref det 'best-eps) 0.0)
        (error "epsilon should be positive"))
      (unless (= (vector-length (hash-ref det 'mu)) 10)
        (error "mu should have 10 elements"))
      
      (let ((testing (hash-ref det 'testing)))
        (when (> (length testing) 0)
          (let* ((s (car testing))
                 (actual (if (> (vector-ref s 9) 0.5) "anomaly" "normal"))
                 (predicted (if (is-anomaly det s) "anomaly" "normal")))
            (displayln (format "\nFirst test sample: actual=~a, predicted=~a" actual predicted)))))
      
      (displayln "All assertions passed.")
      (displayln "=== Test complete ==="))))

(main)
