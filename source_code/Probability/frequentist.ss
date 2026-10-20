;; File: frequentist.ss
;; Frequentist hypothesis-testing toolkit: z-tests, chi-squared tests,
;; and confidence intervals.
;;
;; IMPORTANT CAVEAT: a small p-value tells you the observed data would be
;; unlikely *if* the null hypothesis were true. It does NOT tell you the
;; probability that the null hypothesis is false, nor the probability that
;; your alternative hypothesis is true.

(export phi-approx chi-squared-cdf z-score z-test-proportion
        chi-squared-test chi-squared-independence
        z-critical confidence-interval-proportion)

;; Sum a list of numbers (fold avoids apply's argument-count limit on big lists).
(def (sum xs) (foldl + 0 xs))

(def pi (* 4.0 (atan 1.0)))

;; Approximate the standard normal CDF Phi(z) = P(Z <= z) using the
;; Abramowitz & Stegun 26.2.17 polynomial (max abs error 7.5e-8).
(def (phi-approx z)
  (let* ((p 0.2316419)
         (b1 0.319381530)
         (b2 -0.356563782)
         (b3 1.781477937)
         (b4 -1.821255978)
         (b5 1.330274429)
         (az (abs (exact->inexact z)))
         (t (/ 1.0 (+ 1.0 (* p az))))
         (pdf (/ (exp (* -0.5 az az)) (sqrt (* 2.0 pi))))
         (cdf (- 1.0 (* pdf (+ (* b1 t)
                               (* b2 (expt t 2))
                               (* b3 (expt t 3))
                               (* b4 (expt t 4))
                               (* b5 (expt t 5)))))))
    (if (>= z 0) cdf (- 1.0 cdf))))

;; Approximate the chi-squared CDF P(X <= x) with DF degrees of freedom
;; using the Wilson-Hilferty (1931) normal approximation.
(def (chi-squared-cdf x df)
  (if (<= x 0)
    0.0
    (let* ((k (exact->inexact df))
           (term (/ 2.0 (* 9.0 k)))
           (z (/ (- (expt (/ x k) (/ 1.0 3.0)) (- 1.0 term))
                 (sqrt term))))
      (phi-approx z))))

;; Standard z-score: (OBSERVED - EXPECTED) / STD-DEV.
(def (z-score observed expected std-dev)
  (unless (> std-dev 0) (error "STD-DEV must be positive."))
  (/ (- observed expected) (exact->inexact std-dev)))

;; One-sample two-tailed z-test for a binomial proportion.
;; Tests H0: p = HYPOTHESISED-P. Returns two values: Z-SCORE and P-VALUE.
(def (z-test-proportion successes n hypothesised-p)
  (let* ((p0 (exact->inexact hypothesised-p))
         (p-hat (/ (exact->inexact successes) n))
         (se (sqrt (/ (* p0 (- 1.0 p0)) n)))
         (z (/ (- p-hat p0) se))
         (p-val (* 2.0 (- 1.0 (phi-approx (abs z))))))
    (values z (min p-val 1.0))))

;; Pearson's chi-squared goodness-of-fit test.
;; OBSERVED and EXPECTED are equal-length lists of counts.
;; Returns three values: CHI-SQUARED, DF, and P-VALUE.
(def (chi-squared-test observed expected)
  (unless (= (length observed) (length expected))
    (error "OBSERVED and EXPECTED must have the same length."))
  (let* ((chi2 (sum (map (lambda (o e)
                               (if (zero? e) 0.0
                                   (/ (expt (- o e) 2) (exact->inexact e))))
                             observed expected)))
         (df (- (length observed) 1))
         (p-val (- 1.0 (chi-squared-cdf chi2 df))))
    (values chi2 df (max p-val 0.0))))

;; Pearson's chi-squared test of independence for a contingency TABLE
;; (a list of rows, each a list of counts). Expected counts come from the
;; row and column marginals, so DF = (rows - 1) * (cols - 1).
;; Returns three values: CHI-SQUARED, DF, and P-VALUE.
(def (chi-squared-independence table)
  (let ((rows (length table))
        (cols (length (car table))))
    (unless (and (>= rows 2) (>= cols 2))
      (error "TABLE must have at least two rows and two columns."))
    (let* ((row-totals (map (lambda (row) (sum row)) table))
           (col-totals (apply map + table))   ; column-wise sums
           (n (sum row-totals))
           (chi2 (sum
                  (map (lambda (row row-total)
                         (sum
                          (map (lambda (o col-total)
                                 (let ((e (/ (* row-total col-total)
                                             (exact->inexact n))))
                                   (if (zero? e) 0.0
                                       (/ (expt (- o e) 2) e))))
                               row col-totals)))
                       table row-totals)))
           (df (* (- rows 1) (- cols 1)))
           (p-val (- 1.0 (chi-squared-cdf chi2 df))))
      (values chi2 df (max p-val 0.0)))))

;; Return the z* critical value for a two-sided confidence level.
;; Common levels come from a lookup table; others are found by bisection.
(def (z-critical confidence)
  (let ((entry (find (lambda (pair) (< (abs (- (car pair) confidence)) 1e-6))
                     '((0.90 . 1.6449) (0.95 . 1.9600) (0.99 . 2.5758)))))
    (if entry
      (cdr entry)
      (let ((target (/ (+ 1.0 confidence) 2.0)))
        (let loop ((i 0) (lo 0.0) (hi 5.0))
          (if (= i 60)
            (/ (+ lo hi) 2.0)
            (let ((mid (/ (+ lo hi) 2.0)))
              (if (< (phi-approx mid) target)
                (loop (+ i 1) mid hi)
                (loop (+ i 1) lo mid)))))))))

;; Wilson score confidence interval for a binomial proportion.
;; Returns two values: LOWER and UPPER bounds.
(def (confidence-interval-proportion successes n confidence: (confidence 0.95))
  (let* ((n (exact->inexact n))
         (p (/ successes n))
         (z (z-critical confidence))
         (z2 (* z z))
         (denom (+ 1.0 (/ z2 n)))
         (centre (/ (+ p (/ z2 (* 2.0 n))) denom))
         (margin (/ (* z (sqrt (+ (/ (* p (- 1.0 p)) n)
                                  (/ z2 (* 4.0 n n)))))
                    denom)))
    (values (max 0.0 (- centre margin))
            (min 1.0 (+ centre margin)))))
