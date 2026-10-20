;; File: differentiation.ss
;; Symbolic differentiation of polynomials.
;;
;;   Power rule:   d/dx(c * x^n)  = n*c * x^(n-1)   (n >= 1)
;;                 d/dx(constant) = 0
;;   Sum rule:     d/dx(p + q) = p' + q'
;;                 (automatic, because polynomials are sums of terms)

(import "data")
(export differentiate-term differentiate differentiate-n
        gradient-at critical-point?)

;; Apply the power rule to a single monomial TERM.
;; Returns a new term, or #f when the term differentiates to zero.
(def (differentiate-term term)
  (let ((c (term-coefficient term))
        (v (term-variable term))
        (n (term-exponent term)))
    (if (zero? n)
      #f                              ; constant term -> zero, drop it
      (make-term (* n c) v (- n 1)))))

;; Differentiate polynomial POLY with respect to its variable.
;;   p = 3x^2 - x + 5   =>   p' = 6x - 1
(def (differentiate poly)
  (let ((var (polynomial-variable poly))
        (diff-terms (filter-map differentiate-term (polynomial-terms poly))))
    (if (null? diff-terms)
      (zero-polynomial var domain: (polynomial-domain poly))
      (make-polynomial var diff-terms domain: (polynomial-domain poly)))))

;; Return the N-th derivative of POLY (N = 0 returns POLY unchanged).
(def (differentiate-n poly n)
  (if (zero? n)
    poly
    (differentiate-n (differentiate poly) (- n 1))))

;; Numerical value of the derivative of POLY at VALUE: differentiate
;; symbolically, then evaluate.
(def (gradient-at poly value)
  (polynomial-evaluate (differentiate poly) value))

;; #t if VALUE is (numerically) a critical point of POLY: |p'(VALUE)| < TOLERANCE.
(def (critical-point? poly value tolerance: (tolerance 1e-9))
  (< (abs (gradient-at poly value)) tolerance))
