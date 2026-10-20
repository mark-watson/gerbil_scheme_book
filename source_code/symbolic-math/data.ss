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
     ((= e 1)   (format "~a~a" c v))
     (else      (format "~a~a^~a" c v e)))))

;;; ------------------------------------------------------------------------
;;; 4. sym-polynomial
;;; ------------------------------------------------------------------------

;; Terms are kept in descending order by exponent.
(defstruct sym-polynomial (variable terms domain) transparent: #t)

(def polynomial? sym-polynomial?)
(def polynomial-variable sym-polynomial-variable)
(def polynomial-terms sym-polynomial-terms)
(def polynomial-domain sym-polynomial-domain)

;; Internal: combine like-exponent terms, drop zero coefficients, and return
;; (exponent . coefficient) pairs sorted by descending exponent.
(def (sort-and-combine-terms terms)
  (let ((table (make-hash-table)))
    (for-each (lambda (term)
                (hash-update! table (term-exponent term)
                              (lambda (c) (+ c (term-coefficient term)))
                              0))
              terms)
    (sort (filter (lambda (pair) (not (zero? (cdr pair))))
                  (hash->list table))
          (lambda (a b) (> (car a) (car b))))))

;; Create a polynomial in VARIABLE from a list of TERMS. Like-exponent terms
;; are combined and zero-coefficient terms dropped.
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
