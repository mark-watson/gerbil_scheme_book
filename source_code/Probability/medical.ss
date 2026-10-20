;; File: medical.ss
;; Medical-diagnosis worked example (Bayesian + correlation).
;;
;; A rare disease affects 0.1% of the population. A screening test has
;;   - sensitivity (true-positive rate): 99%
;;   - false-positive rate:               5%
;; If a patient tests positive, what is the probability that they actually
;; have the disease? Intuition says "very high" -- Bayes' Theorem says ~1.9%.

(import :std/format
        "bayes" "correlation" "fmt")
(export prevalence sensitivity false-positive-rate
        medical-likelihood run-bayesian-analysis
        run-correlation-analysis run-medical-example)

(def prevalence 0.001)          ; P(disease)
(def sensitivity 0.99)          ; P(positive | disease)
(def false-positive-rate 0.05)  ; P(positive | healthy)

;; Return P(evidence | hypothesis). The evidence is always a positive test.
(def (medical-likelihood hypothesis evidence)
  (case hypothesis
    ((disease) sensitivity)
    ((healthy) false-positive-rate)
    (else (error "Unknown hypothesis" hypothesis))))

;; Compute posterior probability of disease given a positive test.
(def (run-bayesian-analysis)
  (let* ((prior (make-bayes-model `((disease . ,prevalence)
                                    (healthy . ,(- 1.0 prevalence)))))
         (updated (update prior 'positive-test medical-likelihood)))
    (displayln "\n=== Bayesian Analysis: Medical Screening Test ===")
    (displayln "Prior probabilities:")
    (for-each (lambda (p) (printf "  P(~a) = ~a\n" (car p) (fixed (cdr p) 4)))
              (posteriors prior))
    (displayln "\nAfter a POSITIVE test result:")
    (for-each (lambda (p)
                (printf "  P(~a | positive) = ~a  (~a)\n"
                        (car p) (fixed (cdr p) 4) (percent (cdr p))))
              (posteriors updated))
    (printf "\nMAP hypothesis: ~a\n" (car (maximum-a-posteriori updated)))
    (displayln "\nKey insight: despite 99% sensitivity, a positive test")
    (displayln "only yields about 1.9% probability of disease because the")
    (displayln "disease is so rare (0.1% prevalence). This is exactly")
    (displayln "the kind of counter-intuitive result Bayes' Theorem reveals.")
    updated))

;; Generate N individuals; return (TEST-RESULTS . ACTUAL-DIAGNOSES) as 0/1 lists.
(def (generate-synthetic-population (n 10000))
  (let loop ((i 0) (tests '()) (diagnoses '()))
    (if (= i n)
      (cons tests diagnoses)
      (let* ((has-disease (< (random-real) prevalence))
             (test-positive (< (random-real)
                               (if has-disease sensitivity false-positive-rate))))
        (loop (+ i 1)
              (cons (if test-positive 1.0 0.0) tests)
              (cons (if has-disease 1.0 0.0) diagnoses))))))

;; Compute Pearson r between test results and actual disease status.
(def (run-correlation-analysis)
  (let* ((pop (generate-synthetic-population 100000))
         (tests (car pop))
         (diagnoses (cdr pop))
         (r (pearson-r tests diagnoses)))
    (printf "\n=== Correlation Analysis (N = ~a) ===\n" (with-commas (length tests)))
    (printf "Pearson r(test-result, disease) = ~a\n" (fixed r 4))
    (displayln "\nThis positive correlation is real but modest. It shows")
    (displayln "that the test result and disease status are associated,")
    (displayln "but the correlation coefficient alone cannot tell you the")
    (displayln "probability that any *individual* patient is sick -- that")
    (displayln "requires Bayesian reasoning with the base rate (prevalence).")
    r))

(def (run-medical-example)
  (run-bayesian-analysis)
  (run-correlation-analysis)
  (displayln "\n=== Done. ==="))
