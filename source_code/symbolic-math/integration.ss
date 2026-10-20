;; File: integration.ss
;; Symbolic integration of polynomials.
;;
;;   Reverse power rule:  integral of c * x^n dx = (c / (n+1)) * x^(n+1) + C
;;   Sum rule:            integral of (p + q) = integral p + integral q
;;   Fundamental theorem: integral over [a,b] of f(x) dx = F(b) - F(a)
;;                        where F is the antiderivative of f

(import "data")
(export integrate-term integrate integrate-n
        evaluate-definite evaluate-integral
        make-indefinite-integral make-definite-integral)

;; Apply the reverse power rule to a single monomial TERM.
;; Scheme keeps c/(n+1) as an exact rational, e.g. -1/2.
(def (integrate-term term)
  (let ((new-exp (+ (term-exponent term) 1)))
    (make-term (/ (term-coefficient term) new-exp)
               (term-variable term)
               new-exp)))

;; Antiderivative of POLY (without the constant of integration +C).
;;   p = 3x^2 - x + 5   =>   F = x^3 - (1/2)x^2 + 5x
(def (integrate poly)
  (let ((var (polynomial-variable poly))
        (new-terms (map integrate-term (polynomial-terms poly))))
    (if (null? new-terms)
      (zero-polynomial var domain: (polynomial-domain poly))
      (make-polynomial var new-terms domain: (polynomial-domain poly)))))

;; N-th iterated antiderivative of POLY.
(def (integrate-n poly n)
  (if (zero? n)
    poly
    (integrate-n (integrate poly) (- n 1))))

;; Internal: convert a bound (number or sym-constant) to a number.
(def (resolve-bound bound)
  (cond
   ((not bound) (error "Bound is #f -- use evaluate-definite only on definite integrals."))
   ((number? bound) bound)
   ((constant? bound) (constant-numeric-value bound))
   (else (error "Unrecognised bound type:" bound))))

;; Evaluate the definite integral of POLY over [LOWER, UPPER] using the
;; Fundamental Theorem of Calculus. Bounds may be numbers or sym-constants.
;; Exact bounds give exact results: (evaluate-definite p 0 1) => 11/2
(def (evaluate-definite poly lower upper)
  (let ((F (integrate poly)))
    (- (polynomial-evaluate F (resolve-bound upper))
       (polynomial-evaluate F (resolve-bound lower)))))

;; Numeric value of a definite sym-integral.
(def (evaluate-integral integral)
  (unless (integral-definite? integral)
    (error "INTEGRAL is indefinite; only definite integrals have a numeric value."))
  (unless (polynomial? (integral-integrand integral))
    (error "INTEGRAL's integrand must be a sym-polynomial"))
  (evaluate-definite (integral-integrand integral)
                     (integral-lower integral)
                     (integral-upper integral)))

;; Wrap POLY in an indefinite integral shell:  ∫(expr) dx
(def (make-indefinite-integral poly)
  (make-integral poly (polynomial-variable poly)))

;; Wrap POLY with explicit bounds:  ∫[a,b](expr) dx
(def (make-definite-integral poly lower upper)
  (make-integral poly (polynomial-variable poly) lower: lower upper: upper))
