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
