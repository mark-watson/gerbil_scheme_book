;;; pi.ss -- estimate the value of pi five different ways.
;;;
;;;   gxi pi.ss                    ; default workload
;;;   gxi pi.ss -n 1000000         ; more Monte Carlo samples
;;;   gxi pi.ss -t 500000 -d 40    ; more series terms, 40 printed digits
;;;   gxi pi.ss -m mc,machin       ; only two methods
;;;   gxi pi.ss --help
;;;
;;; The Monte Carlo and series methods work in ordinary double precision, so
;;; they stop being interesting after ~15 digits.  Machin's formula is
;;; evaluated in exact rational arithmetic and then turned into a decimal
;;; string digit by digit, so -d may be larger than 15; the first 100 digits
;;; are checked against a reference string.
;;;
;;; See "Pi - Unleashed" (Arndt & Haenel) for the theory behind each method.

(export main)

(import :std/format
        :std/srfi/13
        :std/cli/getopt
        :std/cli/print-exit)

;; ---------------------------------------------------------------------------
;; what we are aiming at

(def true-pi (acos -1.0))

(def pi-first-100-digits
  (string-append "3."
                 "14159265358979323846264338327950288419716939937510"
                 "58209749445923078164062862089986280348253421170679"))

;; ---------------------------------------------------------------------------
;; Monte Carlo: hit-or-miss in the unit square
;;
;; The quarter circle x^2 + y^2 <= 1 fills pi/4 of the unit square, so a random
;; point is inside it with probability pi/4.  The error shrinks like 1/sqrt(n):
;; six digits would want ~10^12 samples.

(def (pi-monte-carlo samples)
  (let loop ((i 0) (hits 0))
    (cond
     ((= i samples)
      (if (zero? hits) +nan.0 (* 4.0 (/ hits samples))))
     (else
      (let ((x (random-real))
            (y (random-real)))
        (loop (+ i 1) (if (<= (+ (* x x) (* y y)) 1.0) (+ hits 1) hits)))))))

;; ---------------------------------------------------------------------------
;; Buffon's needle
;;
;; Drop a needle of length 1 on a floor with lines 2 apart: it crosses a line
;; with probability 2L/(pi*d) = 1/pi, so pi ~ drops/crossings.  With the
;; symmetry of the floor we only need the needle centre's distance y to the
;; nearest lower line, y in [0,1], and its angle, uniformly in [0, pi/2].

(def (pi-buffon drops)
  (let loop ((i 0) (hits 0))
    (cond
     ((= i drops)
      (if (zero? hits) +nan.0 (/ (exact->inexact drops) (exact->inexact hits))))
     (else
      (let ((y (random-real))
            (theta (* 0.5 true-pi (random-real))))
        (loop (+ i 1)
              (if (> (+ y (* 0.5 (sin theta))) 1.0) (+ hits 1) hits)))))))

;; ---------------------------------------------------------------------------
;; Gregory-Leibniz: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...
;;
;; Correct but glacial: n terms give about log10(n) digits.

(def (pi-leibniz terms)
  (* 4.0
     (let loop ((k 0) (acc 0.0))
       (if (= k terms)
         acc
         (loop (+ k 1)
               (+ acc (/ (if (even? k) 1.0 -1.0)
                         (exact->inexact (+ (* 2 k) 1)))))))))

;; ---------------------------------------------------------------------------
;; Nilakantha: pi = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - ...
;;
;; Same idea, but the terms shrink like 1/n^3, so a few thousand terms are
;; already better than double precision.

(def (pi-nilakantha terms)
  (let loop ((k 1) (acc 3.0))
    (if (> k terms)
      acc
      (let ((denom (* 2.0 k (+ (* 2 k) 1) (+ (* 2 k) 2))))
        (loop (+ k 1) (+ acc (/ (if (odd? k) 4.0 -4.0) denom)))))))

;; ---------------------------------------------------------------------------
;; Machin: pi = 16*arctan(1/5) - 4*arctan(1/239), with the arctan series
;;
;;   arctan(1/x) = 1/x - 1/(3x^3) + 1/(5x^5) - ...
;;
;; evaluated in exact rationals.  Each term of arctan(1/5) buys about
;; log10(25) = 1.4 decimal digits, and the 1/239 part converges much faster.

(def (arctan-inverse x terms)
  (let ((x2 (* x x)))
    (let loop ((k 0) (p x) (acc 0))        ; p = x^(2k+1)
      (if (= k terms)
        acc
        (let ((term (/ 1 (* p (+ (* 2 k) 1)))))
          (loop (+ k 1)
                (* p x2)
                (if (even? k) (+ acc term) (- acc term))))))))

(def (pi-machin terms)
  (- (* 16 (arctan-inverse 5 terms))
     (* 4 (arctan-inverse 239 terms))))

(def (machin-terms-for digits)
  (max 2 (inexact->exact (ceiling (/ (+ digits 4) 1.398)))))

;; ---------------------------------------------------------------------------
;; exact rational -> decimal string, with the given number of fraction digits

(def (rational->decimal-string q digits)
  (let* ((scale (expt 10 digits))
         (scaled (round (* (abs q) scale)))     ; exact integer
         (ip (quotient scaled scale))
         (fp (- scaled (* ip scale)))
         (frac (number->string fp))
         (pad (make-string (max 0 (- digits (string-length frac))) #\0))
         (sign (if (negative? q) "-" "")))
    (string-append sign (number->string ip) "." pad frac)))

(def (matching-prefix-length a b)
  (let ((n (min (string-length a) (string-length b))))
    (let loop ((i 0))
      (cond
       ((= i n) n)
       ((char=? (string-ref a i) (string-ref b i)) (loop (+ i 1)))
       (else i)))))

;; ---------------------------------------------------------------------------
;; output

(def (report name estimate)
  (let* ((e (exact->inexact estimate))
         (err (abs (- e true-pi))))
    (printf "~22a  ~20,15f  ~a\n" name e err)))

(def (header)
  (printf "~22a  ~20a  ~a\n" "method" "estimate" "|estimate - pi|"))

(def (usage)
  (print-exit 2
              (string-append
               "usage: gxi pi.ss [-n SAMPLES] [-t TERMS] [-d DIGITS] [-m LIST]\n"
               "  -n, --samples SAMPLES   Monte Carlo samples (default 200000)\n"
               "  -t, --terms TERMS       series terms for Leibniz/Nilakantha (default 100000)\n"
               "  -d, --digits DIGITS     digits printed by the Machin method (default 30)\n"
               "  -m, --methods LIST      comma separated subset of mc, buffon, leibniz,\n"
               "                          nilakantha, machin (default: all)\n"
               "  -h, --help              this message\n")))

(def (estimate-pi samples terms digits method-list)
  (let* ((wanted (map string-trim-both (string-split method-list #\,)))
         (want? (lambda (name) (and (member name wanted) #t))))
    (header)
    (when (want? "mc") (report "Monte Carlo" (pi-monte-carlo samples)))
    (when (want? "buffon") (report "Buffon's needle" (pi-buffon samples)))
    (when (want? "leibniz") (report "Gregory-Leibniz" (pi-leibniz terms)))
    (when (want? "nilakantha") (report "Nilakantha" (pi-nilakantha terms)))
    (when (want? "machin")
      (let* ((n (machin-terms-for digits))
             (text (rational->decimal-string (pi-machin n) digits))
             (compared (min (string-length text) (string-length pi-first-100-digits)))
             (matched (matching-prefix-length text pi-first-100-digits)))
        (printf "~22a  ~a\n" "Machin (exact rationals)" text)
        (printf "~22a  ~a of the first ~a digits match the reference (~a terms)\n"
                "  check" matched compared n)))))

(def (main . argv)
  (let* ((parser (getopt
                  (option 'samples "-n" "--samples"
                          help: "Monte Carlo samples"
                          default: "200000"
                          value: string->number)
                  (option 'terms "-t" "--terms"
                          help: "series terms for Leibniz/Nilakantha"
                          default: "100000"
                          value: string->number)
                  (option 'digits "-d" "--digits"
                          help: "digits printed by the Machin method"
                          default: "30"
                          value: string->number)
                  (option 'methods "-m" "--methods"
                          help: "comma separated list of methods"
                          default: "mc,buffon,leibniz,nilakantha,machin"
                          value: identity)
                  (flag 'help "-h" "--help" help: "show this message")))
         (opts (getopt-parse parser argv)))
    (when (hash-get opts 'help) (usage))
    (estimate-pi (hash-get opts 'samples)
                 (hash-get opts 'terms)
                 (hash-get opts 'digits)
                 (hash-get opts 'methods))
    0))
