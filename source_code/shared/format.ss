;; File: format.ss
;; Shared number formatting utilities for Gerbil Scheme projects.
(export format-decimal fixed percent with-commas)

(def (format-decimal x (places 2))
  (cond
   ((nan? x) "+nan.0")
   ((= x +inf.0) "+inf.0")
   ((= x -inf.0) "-inf.0")
   (else
    (let* ((neg? (< x 0))
           (scaled (inexact->exact (round (* (abs x) (expt 10 places)))))
           (int-part (quotient scaled (expt 10 places)))
           (frac-part (remainder scaled (expt 10 places)))
           (frac-str (number->string frac-part))
           (padded (string-append (make-string (max 0 (- places (string-length frac-str))) #\0)
                                  frac-str)))
      (string-append (if (and neg? (> scaled 0)) "-" "")
                     (number->string int-part)
                     (if (> places 0) (string-append "." padded) ""))))))

(def fixed format-decimal)

(def (percent p (places 2))
  (string-append (format-decimal (* 100.0 p) places) "%"))

(def (with-commas n)
  (let loop ((s (number->string n)) (acc ""))
    (if (<= (string-length s) 3)
      (string-append s acc)
      (let ((k (- (string-length s) 3)))
        (loop (substring s 0 k)
              (string-append "," (substring s k (string-length s)) acc))))))
