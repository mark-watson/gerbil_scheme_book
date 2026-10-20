;; File: frequentist-demo.ss
;; Re-examines the medical screening scenario from a frequentist standpoint:
;;   1. Simulate a clinical trial (N = 100,000).
;;   2. Chi-squared test of independence between test result and disease.
;;   3. Wilson confidence interval for positive predictive value (PPV).
;;   4. Z-test of the positive-test rate against prevalence.
;;   5. Side-by-side comparison with the Bayesian posterior.
;;
;; Takeaway: a tiny p-value ("highly significant!") coexists with a PPV of
;; only ~2%. Statistical significance is not practical utility.

(import :std/format
        "bayes" "frequentist" "medical" "fmt")
(export simulate-screening run-frequentist-demo)

;; Simulate N individuals screened for the rare disease.
;; Returns four values: TP FP TN FN.
(def (simulate-screening (n 100000))
  (let loop ((i 0) (tp 0) (fp 0) (tn 0) (fn 0))
    (if (= i n)
      (values tp fp tn fn)
      (let* ((sick (< (random-real) prevalence))
             (pos (< (random-real) (if sick sensitivity false-positive-rate))))
        (cond
         ((and sick pos)       (loop (+ i 1) (+ tp 1) fp tn fn))
         (sick                 (loop (+ i 1) tp fp tn (+ fn 1)))
         (pos                  (loop (+ i 1) tp (+ fp 1) tn fn))
         (else                 (loop (+ i 1) tp fp (+ tn 1) fn)))))))

(def (format-p p)
  (if (< p 1e-15) "< 1e-15 (essentially zero)" (format "= ~a" p)))

(def (run-frequentist-demo)
  (displayln "\n================================================================")
  (displayln "  FREQUENTIST ANALYSIS: Medical Screening Test")
  (displayln "================================================================")
  (let-values (((tp fp tn fn) (simulate-screening 100000)))
    (let ((n (+ tp fp tn fn))
          (positives (+ tp fp)))
      ;; 1. Simulation
      (printf "\n--- 1. Simulated Clinical Trial (N = ~a) ---\n" (with-commas n))
      (printf "  True  Positives (TP): ~a\n" tp)
      (printf "  False Positives (FP): ~a\n" fp)
      (printf "  True  Negatives (TN): ~a\n" tn)
      (printf "  False Negatives (FN): ~a\n" fn)

      ;; 2. Chi-squared test of independence
      ;;              Disease+   Disease-
      ;;   Test+        TP         FP
      ;;   Test-        FN         TN
      (displayln "\n--- 2. Chi-Squared Test of Independence ---")
      (let-values (((chi2 df p-val)
                    (chi-squared-independence (list (list tp fp) (list fn tn)))))
        (printf "  chi-squared = ~a   df = ~a   p-value ~a\n"
                (fixed chi2 2) df (format-p p-val))
        (displayln "\n  Interpretation: the test result and disease status")
        (displayln "  are NOT independent (we reject H0). But this only")
        (displayln "  means the *association exists* -- it says nothing about")
        (displayln "  how strong it is or what it means for one patient."))

      ;; 3. Confidence interval for PPV
      (let ((ppv (if (zero? positives) 0.0 (/ tp (exact->inexact positives)))))
        (displayln "\n--- 3. Positive Predictive Value (PPV) ---")
        (printf "  PPV = TP / (TP + FP) = ~a / ~a = ~a  (~a)\n"
                tp positives (fixed ppv 4) (percent ppv))
        (let-values (((lo hi) (confidence-interval-proportion tp positives
                                                               confidence: 0.95)))
          (printf "  95% Wilson CI for PPV: [~a, ~a]  (~a - ~a)\n"
                  (fixed lo 4) (fixed hi 4) (percent lo) (percent hi))))

      ;; 4. Z-test: is the positive-test rate significantly > prevalence?
      (displayln "\n--- 4. Z-Test: Positive Rate vs. Prevalence ---")
      (let-values (((z p-val) (z-test-proportion positives n prevalence)))
        (printf "  Observed positive rate: ~a\n" (percent (/ positives (exact->inexact n)) 4))
        (printf "  Hypothesised rate (prevalence): ~a\n" (percent prevalence 4))
        (printf "  z = ~a   p-value ~a\n" (fixed z 4) (format-p p-val))
        (displayln "\n  The positive rate far exceeds the disease prevalence")
        (displayln "  because of the 5% false-positive rate -- most positives")
        (displayln "  are healthy people."))

      ;; 5. Contrast with the Bayesian result
      (displayln "\n--- 5. Bayesian vs. Frequentist Side-by-Side ---")
      (let* ((prior (make-bayes-model `((disease . ,prevalence)
                                        (healthy . ,(- 1.0 prevalence)))))
             (updated (update prior 'positive-test medical-likelihood))
             (p-disease (posterior updated 'disease))
             (ppv (/ tp (exact->inexact positives))))
        (printf "  Bayesian posterior P(disease | positive test) = ~a  (~a)\n"
                (fixed p-disease 4) (percent p-disease))
        (printf "  Frequentist PPV from simulation             = ~a  (~a)\n"
                (fixed ppv 4) (percent ppv)))

      (displayln "\n  Both frameworks agree: a positive test on a rare disease")
      (displayln "  gives only about 2% probability of actual illness.")
      (displayln "\n================================================================")
      (displayln "  Key lesson: statistical significance (small p-value) and")
      (displayln "  practical significance (high PPV) are different things.")
      (displayln "================================================================"))))
