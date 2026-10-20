;; File: fmt.ss
;; Small number-formatting helpers. Gerbil's :std/format does not support
;; Common Lisp style "~,4F" precision directives, so we provide our own.
(export fixed percent with-commas)

;; Format a real number X with exactly PLACES digits after the decimal point.
;; Example: (fixed 3.14159 2) => "3.14", (fixed 0.5 3) => "0.500"
(def (fixed x (places 4))
  (let* ((neg? (< x 0))
         (scaled (inexact->exact (round (* (abs x) (expt 10 places)))))
         (int-part (quotient scaled (expt 10 places)))
         (frac-part (remainder scaled (expt 10 places)))
         (frac-str (number->string frac-part))
         (padded (string-append (make-string (- places (string-length frac-str)) #\0)
                                frac-str)))
    (string-append (if (and neg? (> scaled 0)) "-" "")
                   (number->string int-part)
                   (if (> places 0) (string-append "." padded) ""))))

;; Format a probability P (0..1) as a percentage string with PLACES decimals.
(def (percent p (places 2))
  (string-append (fixed (* 100.0 p) places) "%"))

;; Format an integer with thousands separators: 100000 => "100,000"
(def (with-commas n)
  (let loop ((s (number->string n)) (acc ""))
    (if (<= (string-length s) 3)
      (string-append s acc)
      (let ((k (- (string-length s) 3)))
        (loop (substring s 0 k)
              (string-append "," (substring s k (string-length s)) acc))))))
