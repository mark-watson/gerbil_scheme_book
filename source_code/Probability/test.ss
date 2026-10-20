;; File: test.ss
;; Runs the Bayesian, correlation, and frequentist demos plus a few sanity checks.
(import :std/format
        "bayes" "correlation" "frequentist" "medical" "frequentist-demo" "fmt")

(def (check name ok?)
  (printf "  [~a] ~a\n" (if ok? "PASS" "FAIL") name))

(displayln "=== Sanity checks ===")
(check "Phi(0) = 0.5" (< (abs (- (phi-approx 0) 0.5)) 1e-6))
(check "Phi(1.96) ~ 0.975" (< (abs (- (phi-approx 1.96) 0.975)) 1e-4))
(check "Pearson r of perfectly linear data = 1"
       (< (abs (- (pearson-r '(1 2 3 4) '(2 4 6 8)) 1.0)) 1e-9))
(check "Spearman rho of monotonic non-linear data = 1"
       (< (abs (- (spearman-rho '(1 2 3 4) '(1 8 27 64)) 1.0)) 1e-9))
(check "rank-list averages ties" (equal? (rank-list '(10 20 20 30)) '(1. 2.5 2.5 4.)))
(let-values (((chi2 df p) (chi-squared-test '(50 50) '(50 50))))
  (check "chi-squared of perfect fit = 0" (and (= chi2 0) (= df 1))))

(displayln "\nCorrelation matrix example:")
(for-each (lambda (triple)
            (printf "  r(~a, ~a) = ~a\n" (car triple) (cadr triple) (fixed (cddr triple) 3)))
          (correlation-matrix '((height . (150 160 170 180 190))
                                (weight . (50 58 69 77 90))
                                (shoe   . (42 38 44 40 41)))))

(run-medical-example)
(run-frequentist-demo)
