# Symbolic Mathematics: Polynomial Differentiation and Integration

Symbolic mathematics has been a central pillar of Lisp and functional programming since the earliest days of artificial intelligence. In 1958, John McCarthy invented Lisp at MIT in part to support symbolic differentiation and algebraic manipulation. Unlike numerical algorithms that approximate derivatives using finite differences or approximate integrals using quadrature sums, symbolic algebra manipulates mathematical expressions directly as structured data. 

A symbolic algebra system computes exact closed-form expressions. When you differentiate `3x^2 - x + 5`$, the system yields the polynomial `6x - 1`$. When you integrate that same polynomial over the interval `[0, 1]`$, it applies the Fundamental Theorem of Calculus to compute the exact rational value `11/2`$, without any floating-point rounding error.

In this chapter, we construct a complete symbolic calculus engine for single-variable polynomials in pure Gerbil Scheme. The system relies entirely on the Gerbil standard library and the Gambit numeric tower, using `defstruct` records to model mathematical primitives with exact rational arithmetic.

## Theoretical Foundations

Before building the data representations, we establish the underlying calculus principles.

### Polynomial Representation and Monomials

A single-variable polynomial `P(x)`$ of degree `n`$ in the variable `x`$ is an expression of the form:

```$
P(x) = \sum_{k=0}^n c_k x^k = c_n x^n + c_{n-1} x^{n-1} + \dots + c_1 x + c_0
```

where each `c_k`$ is a coefficient and each `k`$ is a non-negative integer exponent. 

Each building block `c_k x^k`$ is a *monomial* (or term). To maintain a canonical form, polynomials must combine like terms having identical exponents, discard terms with zero coefficients, and sort terms in descending order by exponent.

### Symbolic Differentiation: The Power Rule

Differentiation is a linear operator on polynomials. Because `\frac{d}{dx}[P(x) + Q(x)] = \frac{d}{dx}P(x) + \frac{d}{dx}Q(x)`$, we differentiate a full polynomial by differentiating each monomial term independently and summing the results.

For any single monomial term, the power rule states:

```$
\frac{d}{dx}(c \cdot x^k) = (k \cdot c) \cdot x^{k-1} \quad (k \ge 1)
```

For a constant term where `k = 0`$, the derivative is zero:

```$
\frac{d}{dx}(c \cdot x^0) = 0
```

Higher-order derivatives `P^{(n)}(x)`$ are obtained by iteratively differentiating `n`$ times. A critical point of `P(x)`$ occurs at any value `x_0`$ where the first derivative vanishes: `P'(x_0) = 0`$.

### Symbolic Integration: The Reverse Power Rule

Integration is the inverse operation of differentiation. The indefinite integral (or antiderivative) of a polynomial is determined by the reverse power rule:

```$
\int c \cdot x^k \, dx = \left(\frac{c}{k + 1}\right) x^{k + 1} + C
```

Here Scheme's exact rational arithmetic provides a distinct advantage. If `c = 1`$ and `k = 1`$, the coefficient becomes the exact rational number `1/2`$ rather than the inexact floating-point value `0.5`$.

By the Fundamental Theorem of Calculus, the definite integral of `P(x)`$ from lower limit `a`$ to upper limit `b`$ is computed from its antiderivative `F(x)`$:

```$
\int_a^b P(x) \, dx = F(b) - F(a)
```

When `a`$ and `b`$ are exact numbers, the resulting definite integral is also exact.

## The Data Layer (`data.ss`)

The `data.ss` module establishes the core mathematical data structures using `defstruct`. It defines representations for variables, named constants, monomial terms, polynomials, and integrals, along with normalization and arithmetic operations.

```scheme
;; File: data.ss
;; Core data structures for symbolic mathematics.
;;
;;   sym-variable   -- a symbolic variable (e.g. x, y, t)
;;   sym-constant   -- a named constant (e.g. pi, e, 42)
;;   sym-term       -- a single monomial: coefficient * variable ^ exponent
;;   sym-polynomial -- a polynomial in one variable (ordered list of terms)
;;   sym-integral   -- a definite or indefinite integral
;;
;; Coefficients may be any Scheme real, including exact rationals such as 1/2,
;; so integration results stay exact (just as in Common Lisp).

(import :std/sort :std/format)
(export make-variable variable? variable-name variable-domain variable=?
        make-constant constant? constant-name constant-value constant-numeric-value
        make-term term? term-coefficient term-variable term-exponent
        term=? term-negate term-scale term->string
        make-polynomial polynomial? polynomial-variable polynomial-terms
        polynomial-domain polynomial-degree polynomial-leading-term
        polynomial-add polynomial-subtract polynomial-negate polynomial-scale
        polynomial-evaluate polynomial->string polynomial-normalize
        zero-polynomial constant-polynomial identity-polynomial
        make-integral integral? integral-integrand integral-variable
        integral-lower integral-upper integral-definite? integral->string)

;;; ------------------------------------------------------------------------
;;; 1. sym-variable
;;; ------------------------------------------------------------------------

(defstruct sym-variable (name domain) transparent: #t)

;; Create a symbolic variable with NAME (a symbol) and optional DOMAIN,
;; one of 'real (default), 'complex, or 'integer.
(def (make-variable name domain: (domain 'real))
  (unless (symbol? name) (error "Variable name must be a symbol:" name))
  (unless (memq domain '(real complex integer))
    (error "DOMAIN must be real, complex, or integer:" domain))
  (make-sym-variable name domain))

(def variable? sym-variable?)
(def variable-name sym-variable-name)
(def variable-domain sym-variable-domain)

;; Two variables are equal when their names and domains match.
(def (variable=? a b)
  (and (variable? a) (variable? b)
       (eq? (variable-name a) (variable-name b))
       (eq? (variable-domain a) (variable-domain b))))

;;; ------------------------------------------------------------------------
;;; 2. sym-constant
;;; ------------------------------------------------------------------------

(defstruct sym-constant (name value) transparent: #t)

;; Create a named constant. VALUE may be a real number, 'pi, or 'e.
(def (make-constant name value)
  (unless (or (real? value) (memq value '(pi e)))
    (error "VALUE must be a number, 'pi, or 'e:" value))
  (make-sym-constant name value))

(def constant? sym-constant?)
(def constant-name sym-constant-name)
(def constant-value sym-constant-value)

;; Return the numeric (floating-point) value of constant C.
(def (constant-numeric-value c)
  (case (constant-value c)
    ((pi) (* 4.0 (atan 1.0)))
    ((e) (exp 1.0))
    (else (constant-value c))))

;;; ------------------------------------------------------------------------
;;; 3. sym-term  (monomial: coefficient * variable ^ exponent)
;;; ------------------------------------------------------------------------

(defstruct sym-term (coefficient variable exponent) transparent: #t)

;; Create a monomial COEFFICIENT * VARIABLE ^ EXPONENT (EXPONENT >= 0).
;;   (make-term 3 x 2)   ; 3x^2
;;   (make-term 5 x 0)   ; constant 5
(def (make-term coefficient variable exponent)
  (unless (real? coefficient) (error "Coefficient must be real:" coefficient))
  (unless (variable? variable) (error "Not a sym-variable:" variable))
  (unless (and (exact-integer? exponent) (>= exponent 0))
    (error "Exponent must be a non-negative integer:" exponent))
  (make-sym-term coefficient variable exponent))

(def term? sym-term?)
(def term-coefficient sym-term-coefficient)
(def term-variable sym-term-variable)
(def term-exponent sym-term-exponent)

(def (term=? a b)
  (and (term? a) (term? b)
       (= (term-coefficient a) (term-coefficient b))
       (variable=? (term-variable a) (term-variable b))
       (= (term-exponent a) (term-exponent b))))

(def (term-negate term)
  (term-scale term -1))

(def (term-scale term scalar)
  (make-sym-term (* scalar (term-coefficient term))
                 (term-variable term)
                 (term-exponent term)))

;; Human-readable form, e.g. "3x^2", "-1x", "5".
(def (term->string term)
  (let ((c (term-coefficient term))
        (v (symbol->string (variable-name (term-variable term))))
        (e (term-exponent term)))
    (cond
     ((zero? e) (format "~a" c))
     ((= e 1)   (format "~ax" c))
     (else      (format "~ax^~a" c e)))))

;;; ------------------------------------------------------------------------
;;; 4. sym-polynomial
;;; ------------------------------------------------------------------------

(defstruct sym-polynomial (variable terms domain) transparent: #t)

(def polynomial? sym-polynomial?)
(def polynomial-variable sym-polynomial-variable)
(def polynomial-terms sym-polynomial-terms)
(def polynomial-domain sym-polynomial-domain)

;; Internal helper: group like exponents, sum coefficients, drop zeros,
;; and sort terms in descending order by exponent.
(def (sort-and-combine-terms terms)
  (let ((ht (make-hash-table)))
    (for-each (lambda (term)
                (hash-update! ht (term-exponent term) (lambda (c) (+ c (term-coefficient term))) 0))
              terms)
    (let ((combined '()))
      (hash-for-each (lambda (exp coeff)
                       (unless (zero? coeff)
                         (set! combined (cons (cons exp coeff) combined))))
                     ht)
      (sort combined (lambda (a b) (> (car a) (car b)))))))

;; Create a polynomial in VARIABLE from a list of TERMS.
(def (make-polynomial variable terms domain: (domain 'real))
  (unless (variable? variable) (error "Not a sym-variable:" variable))
  (for-each (lambda (term)
              (unless (variable=? variable (term-variable term))
                (error "Term variable does not match polynomial variable:"
                       (variable-name (term-variable term))
                       (variable-name variable))))
            terms)
  (unless (memq domain '(real complex))
    (error "DOMAIN must be real or complex:" domain))
  (make-sym-polynomial
   variable
   (map (lambda (pair) (make-sym-term (cdr pair) variable (car pair)))
        (sort-and-combine-terms terms))
   domain))

(def (polynomial-normalize poly)
  (make-polynomial (polynomial-variable poly) (polynomial-terms poly)
                   domain: (polynomial-domain poly)))

;; Degree (highest exponent) of POLY; -1 for the zero polynomial.
(def (polynomial-degree poly)
  (if (null? (polynomial-terms poly))
    -1
    (term-exponent (car (polynomial-terms poly)))))

(def (polynomial-leading-term poly)
  (and (pair? (polynomial-terms poly)) (car (polynomial-terms poly))))

;; Arithmetic happens in the wider of two domains (real is a subset of complex).
(def (join-domains a b)
  (if (or (eq? a 'complex) (eq? b 'complex)) 'complex 'real))

(def (polynomial-add p q)
  (unless (variable=? (polynomial-variable p) (polynomial-variable q))
    (error "Cannot add polynomials in different variables"))
  (make-polynomial (polynomial-variable p)
                   (append (polynomial-terms p) (polynomial-terms q))
                   domain: (join-domains (polynomial-domain p) (polynomial-domain q))))

(def (polynomial-negate poly)
  (polynomial-scale poly -1))

(def (polynomial-subtract p q)
  (polynomial-add p (polynomial-negate q)))

(def (polynomial-scale poly scalar)
  (make-polynomial (polynomial-variable poly)
                   (map (lambda (term) (term-scale term scalar))
                        (polynomial-terms poly))
                   domain: (polynomial-domain poly)))

;; Evaluate POLY at VALUE by substituting the variable.
(def (polynomial-evaluate poly value)
  (foldl (lambda (term acc)
           (+ acc (* (term-coefficient term) (expt value (term-exponent term)))))
         0
         (polynomial-terms poly)))

;; e.g. "3x^2 + -1x + 5"; the zero polynomial renders as "0".
(def (polynomial->string poly)
  (if (null? (polynomial-terms poly))
    "0"
    (let loop ((terms (cdr (polynomial-terms poly)))
               (acc (term->string (car (polynomial-terms poly)))))
      (if (null? terms)
        acc
        (loop (cdr terms) (string-append acc " + " (term->string (car terms))))))))

;;; Convenience constructors

(def (zero-polynomial variable domain: (domain 'real))
  (make-sym-polynomial variable '() domain))

(def (constant-polynomial variable value)
  (make-polynomial variable (list (make-term value variable 0))))

(def (identity-polynomial variable)
  (make-polynomial variable (list (make-term 1 variable 1))))

;;; ------------------------------------------------------------------------
;;; 5. sym-integral
;;; ------------------------------------------------------------------------

;; INTEGRAND is usually a sym-polynomial. LOWER/UPPER are numbers,
;; sym-constants, or #f for an indefinite integral.
(defstruct sym-integral (integrand variable lower upper) transparent: #t)

(def integral? sym-integral?)
(def integral-integrand sym-integral-integrand)
(def integral-variable sym-integral-variable)
(def integral-lower sym-integral-lower)
(def integral-upper sym-integral-upper)

(def (make-integral integrand variable lower: (lower #f) upper: (upper #f))
  (unless (variable? variable) (error "Not a sym-variable:" variable))
  (unless (eq? (not lower) (not upper))
    (error "Either both LOWER and UPPER must be provided, or neither."))
  (make-sym-integral integrand variable lower upper))

(def (integral-definite? integral)
  (and (integral-lower integral) #t))

(def (bound->string bound)
  (cond
   ((not bound) "")
   ((number? bound) (number->string bound))
   ((constant? bound)
    (case (constant-value bound)
      ((pi) "π")
      ((e) "e")
      (else (symbol->string (constant-name bound)))))
   (else (format "~a" bound))))

;; Indefinite: "∫(expr) dx"    Definite: "∫[a,b](expr) dx"
(def (integral->string integral)
  (let ((var (symbol->string (variable-name (integral-variable integral))))
        (body (if (polynomial? (integral-integrand integral))
                (polynomial->string (integral-integrand integral))
                (format "~a" (integral-integrand integral))))
        (bounds (if (integral-definite? integral)
                  (format "[~a,~a]" (bound->string (integral-lower integral))
                          (bound->string (integral-upper integral)))
                  "")))
    (format "∫~a(~a) d~a" bounds body var)))
```

Notice that `sort-and-combine-terms` normalizes any list of terms into a canonical representation: identical exponents are combined, terms with zero coefficients vanish, and the remaining monomials are sorted from highest degree to lowest.

## Symbolic Differentiation (`differentiation.ss`)

With polynomial representation in place, differentiation is implemented in `differentiation.ss`. It provides term differentiation, whole-polynomial differentiation, iterative n-th order derivatives, numerical gradient evaluation, and critical point testing.

```scheme
;; File: differentiation.ss
;; Symbolic differentiation of polynomials.
;;
;;   Power rule:   d/dx(c * x^n)  = n*c * x^(n-1)   (n >= 1)
;;                 d/dx(constant) = 0
;;   Sum rule:     d/dx(p + q) = p' + q'

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
```

The function `filter-map` applies `differentiate-term` across all terms and automatically drops any that return `#f` (the constant terms whose derivatives evaluate to zero).

## Symbolic Integration (`integration.ss`)

The module `integration.ss` implements antidifferentiation and definite integration.

```scheme
;; File: integration.ss
;; Symbolic integration of polynomials.
;;
;;   Reverse power rule:  integral of c * x^n dx = (c / (n+1)) * x^(n+1) + C
;;   Sum rule:            integral of (p + q) = integral p + integral q
;;   Fundamental theorem: integral over [a,b] of f(x) dx = F(b) - F(a)

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
```

In `integrate-term`, the division `(/ (term-coefficient term) new-exp)` generates exact Scheme rationals. For instance, integrating `-1x^1` produces a term with coefficient `-1/2`$, preserving infinite precision throughout algebraic manipulation.

## Complete Test Suite (`test.ss`)

The verification driver in `test.ss` exercises polynomial arithmetic, derivatives, critical point checks, antiderivatives, and definite integrals with exact and transcendental bounds:

```scheme
;; File: test.ss
;; Smoke tests for the data layer, differentiation, and integration.
(import :std/format "data" "differentiation" "integration")

(def (show label value) (printf "~a: ~a\n" label value))

(def x (make-variable 'x))
(def pi-c (make-constant 'pi 'pi))

;; p = 3x^2 - x + 5
(def p (make-polynomial x (list (make-term 3 x 2) (make-term -1 x 1) (make-term 5 x 0))))
;; q = x + 2
(def q (make-polynomial x (list (make-term 1 x 1) (make-term 2 x 0))))
;; r = x^4 - 2x^3 + x
(def r (make-polynomial x (list (make-term 1 x 4) (make-term -2 x 3) (make-term 1 x 1))))
;; s = 6x + 2
(def s (make-polynomial x (list (make-term 6 x 1) (make-term 2 x 0))))

(displayln "\n=== Symbolic Math Data Layer ===\n")
(show "p         " (polynomial->string p))
(show "q         " (polynomial->string q))
(show "p + q     " (polynomial->string (polynomial-add p q)))
(show "p - q     " (polynomial->string (polynomial-subtract p q)))
(show "2 * p     " (polynomial->string (polynomial-scale p 2)))
(show "degree(p) " (polynomial-degree p))
(show "p(0)      " (polynomial-evaluate p 0))
(show "p(1)      " (polynomial-evaluate p 1))
(show "p(2)      " (polynomial-evaluate p 2))
(show "indef     " (integral->string (make-integral p x)))
(show "def       " (integral->string (make-integral p x lower: 0 upper: 1)))
(show "pi-int    " (integral->string (make-integral p x lower: 0 upper: pi-c)))

(displayln "\n=== Differentiation ===\n")
(show "p             " (polynomial->string p))
(show "p'            " (polynomial->string (differentiate p)))
(show "p''           " (polynomial->string (differentiate-n p 2)))
(show "p'''          " (polynomial->string (differentiate-n p 3)))
(show "r             " (polynomial->string r))
(show "r'            " (polynomial->string (differentiate r)))
(show "r'' (via n=2) " (polynomial->string (differentiate-n r 2)))
(show "gradient-at(p, 0)       (expected -1)" (gradient-at p 0))
(show "gradient-at(p, 1)       (expected  5)" (gradient-at p 1))
(show "critical-point?(p, 1/6) (expected #t)" (critical-point? p 1/6))
(show "critical-point?(p, 0)   (expected #f)" (critical-point? p 0))

(displayln "\n=== Integration ===\n")
(show "p           " (polynomial->string p))
(show "∫p dx       " (polynomial->string (integrate p)))
(show "∫∫p dx dx   " (polynomial->string (integrate-n p 2)))
(show "s           " (polynomial->string s))
(show "∫s dx       " (polynomial->string (integrate s)))
(show "∫[0,1] p dx  (expected 11/2)" (evaluate-definite p 0 1))
(show "∫[0,1] s dx  (expected 5)   " (evaluate-definite s 0 1))
(show "∫[0,π] p dx                 " (evaluate-definite p 0 pi-c))
(show "indef shell   " (integral->string (make-indefinite-integral p)))
(show "def [0,1]     " (integral->string (make-definite-integral p 0 1)))
(show "def [0,π]     " (integral->string (make-definite-integral p 0 pi-c)))
(show "evaluate shell (expected 11/2)" (evaluate-integral (make-definite-integral p 0 1)))
(show "d/dx ∫p dx == p ?" (equal? (polynomial->string (differentiate (integrate p)))
                                   (polynomial->string p)))
```

## Running the Code and Expected Output

Execute the test suite using `make test`:

```console
$ make test
gxi test.ss

=== Symbolic Math Data Layer ===

p         : 3x^2 + -1x + 5
q         : 1x + 2
p + q     : 3x^2 + 7
p - q     : 3x^2 + -2x + 3
2 * p     : 6x^2 + -2x + 10
degree(p) : 2
p(0)      : 5
p(1)      : 7
p(2)      : 15
indef     : ∫(3x^2 + -1x + 5) dx
def       : ∫[0,1](3x^2 + -1x + 5) dx
pi-int    : ∫[0,π](3x^2 + -1x + 5) dx

=== Differentiation ===

p             : 3x^2 + -1x + 5
p'            : 6x + -1
p''           : 6
p'''          : 0
r             : 1x^4 + -2x^3 + 1x
r'            : 4x^3 + -6x^2 + 1
r'' (via n=2) : 12x^2 + -12x
gradient-at(p, 0)       (expected -1): -1
gradient-at(p, 1)       (expected  5): 5
critical-point?(p, 1/6) (expected #t): #t
critical-point?(p, 0)   (expected #f): #f

=== Integration ===

p           : 3x^2 + -1x + 5
∫p dx       : 1x^3 + -1/2x^2 + 5x
∫∫p dx dx   : 1/4x^4 + -1/6x^3 + 5/2x^2
s           : 6x + 2
∫s dx       : 3x^2 + 2x
∫[0,1] p dx  (expected 11/2): 11/2
∫[0,1] s dx  (expected 5)   : 5
∫[0,π] p dx                 : 41.77943774770411
indef shell   : ∫(3x^2 + -1x + 5) dx
def [0,1]     : ∫[0,1](3x^2 + -1x + 5) dx
def [0,π]     : ∫[0,π](3x^2 + -1x + 5) dx
evaluate shell (expected 11/2): 11/2
d/dx ∫p dx == p ?: #t
```

## Interpreting the Results

The output confirms several crucial algebraic and calculus behaviors:

1. **Exact Rational Retention**: Notice that `∫[0,1] p dx` evaluates to the exact rational fraction `11/2` rather than `5.5`. Similarly, `∫p dx` produces `-1/2x^2`, and double integration produces `1/4x^4 + -1/6x^3 + 5/2x^2`. Exact fractions prevent the accumulation of numerical drift during compound algebraic workflows.
2. **Analytical Gradient Precision**: Evaluating `p'(1/6)` identifies the critical point where `6(1/6) - 1 = 0`. The predicate `(critical-point? p 1/6)` returns `#t` because the derivative is calculated symbolically before substitution.
3. **Calculus Inversion**: The identity check `(d/dx ∫p dx == p)` confirms that differentiating the antiderivative of a polynomial restores the original polynomial verbatim.

## Wrap Up

In this chapter we implemented a symbolic calculus library for single-variable polynomials in Gerbil Scheme. By leveraging Gerbil's `defstruct` system and Gambit's exact numeric tower, we achieved closed-form differentiation and integration without external symbolic algebra packages. The power rule and reverse power rule translate directly into high-order operations on term lists, proving that classic Lisp symbolic computing remains expressive and practical today.

## Optional Practice Problems

1. **Polynomial Multiplication**: Implement `(polynomial-multiply p q)` in `data.ss` by computing the Cauchy product of the term lists, multiplying each pair of monomials and combining like terms.
2. **Root Finding with Newton-Raphson**: Write a function `(polynomial-find-root poly initial-guess (iterations 20))` that uses symbolic differentiation to implement Newton's method: `x_{k+1} = x_k - \frac{P(x_k)}{P'(x_k)}`$.
3. **Multi-variable Extension**: Extend `make-term` and `make-polynomial` to support multi-variable monomials (e.g. `3 x^2 y^3`) and implement partial differentiation `(partial-differentiate poly var)`.
