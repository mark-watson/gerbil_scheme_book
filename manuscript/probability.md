# Probability: Bayesian and Frequentist Reasoning

Reasoning under uncertainty is fundamental to artificial intelligence, machine learning, and data analysis. Whether evaluating diagnostic tests, assessing classification models, or determining whether an experimental treatment works, engineers must make sound statistical judgments.

In data science, two primary philosophies govern statistical inference:

1. **Bayesian Inference**: Models belief as a probability distribution over hypotheses. When new evidence arrives, Bayes' Theorem updates this distribution to produce a posterior probability.
2. **Frequentist Inference**: Interprets probability as the long-run frequency of events in repeated experiments. It relies on null hypothesis significance testing, p-values, and confidence intervals without assigning prior probabilities to hypotheses.

These two schools often seem at odds in textbook presentations. In this chapter, we implement both frameworks in pure Gerbil Scheme and examine a classic medical screening problem from both perspectives side by side. We will observe how both methodologies arrive at the exact same practical conclusion: why a screening test with 99% accuracy can yield only a 2% chance of disease, and why extreme statistical significance does not equate to high predictive value.

## Theoretical Foundations

Before writing Scheme code, we establish the mathematical concepts used across both schools.

### Bayes' Theorem and Posterior Updating

Let `H`$ be a hypothesis (for example, "the patient has the disease") and `E`$ be an observed piece of evidence (such as "the test result is positive"). Bayes' Theorem computes the conditional probability `P(H \mid E)`$:

```$
P(H \mid E) = \frac{P(E \mid H) \cdot P(H)}{P(E)} = \frac{P(E \mid H) \cdot P(H)}{\sum_{h} P(E \mid h) \cdot P(h)}
```

In this expression:
- `P(H)`$ is the **prior probability**: the initial belief before seeing new evidence.
- `P(E \mid H)`$ is the **likelihood**: the probability that evidence `E`$ occurs given hypothesis `H`$.
- `P(E)`$ is the **marginal likelihood** (or evidence): the total probability of seeing `E`$ across all possible mutually exclusive hypotheses `h`$.
- `P(H \mid E)`$ is the **posterior probability**: the updated belief in `H`$ after incorporating `E`$.

The **Maximum a Posteriori (MAP)** decision rule selects the hypothesis with the highest posterior probability:

```$
H_{\mathrm{MAP}} = \operatorname{argmax}_H P(H \mid E)
```

### The Base Rate Fallacy in Medical Testing

Consider a rare disease that affects 0.1% of the population (`P(\mathrm{disease}) = 0.001`$). Suppose a medical laboratory develops an automated test with:
- **Sensitivity** (True Positive Rate): `P(\mathrm{positive} \mid \mathrm{disease}) = 0.99`$
- **False Positive Rate**: `P(\mathrm{positive} \mid \mathrm{healthy}) = 0.05`$

When a patient tests positive, intuition suggests the probability of disease must be near 99%. However, applying Bayes' Theorem reveals:

```$
P(\mathrm{disease} \mid \mathrm{positive}) = \frac{0.99 \times 0.001}{(0.99 \times 0.001) + (0.05 \times 0.999)} \approx \frac{0.00099}{0.00099 + 0.04995} \approx 0.0194 \quad (1.94\%)
```

Because 99.9% of the population is healthy, the 5% false positive rate applied to that large healthy majority produces roughly 50 false positives for every 1 true positive. Ignoring the base rate (`P(\mathrm{disease}) = 0.001`$) is known as the **base rate fallacy**.

### Frequentist Hypothesis Testing and Confidence Intervals

In the frequentist paradigm, we do not assign a prior probability to the patient's health status. Instead, we formulate a **null hypothesis** `H_0`$ (for instance, "the test result is independent of disease status") and collect sample counts:

- True Positives (TP): sick individuals who test positive.
- False Positives (FP): healthy individuals who test positive.
- True Negatives (TN): healthy individuals who test negative.
- False Negatives (FN): sick individuals who test negative.

#### Pearson's Chi-Squared Test of Independence

To test whether the test result and actual disease status are statistically associated, we construct a 2×2 contingency table and compute the chi-squared statistic:

```$
\chi^2 = \sum_{i=1}^r \sum_{j=1}^c \frac{(O_{ij} - E_{ij})^2}{E_{ij}}
```

where `O_{ij}`$ is the observed count in row `i`$, column `j`$, and `E_{ij} = \frac{(\mathrm{RowTotal}_i)(\mathrm{ColTotal}_j)}{N}`$ is the expected count under independence. For a 2×2 table, the degrees of freedom is `(2 - 1) \times (2 - 1) = 1`$.

#### Positive Predictive Value and the Wilson Score Interval

The **Positive Predictive Value (PPV)** is the proportion of positive test results that are true positives:

```$
\mathrm{PPV} = \frac{\mathrm{TP}}{\mathrm{TP} + \mathrm{FP}}
```

Because `\mathrm{PPV}`$ is estimated from a finite sample, we quantify uncertainty using the **Wilson score confidence interval**. Unlike the naive normal approximation `p \pm z \sqrt{p(1-p)/n}`$ which breaks down for proportions near zero or one, the Wilson interval centers around a adjusted proportion:

```$
\tilde{p} = \frac{p + \frac{z^2}{2n}}{1 + \frac{z^2}{n}} \pm \frac{z}{1 + \frac{z^2}{n}} \sqrt{\frac{p(1-p)}{n} + \frac{z^2}{4n^2}}
```

## The Bayesian Engine (`bayes.ss`)

The module `bayes.ss` provides a general Bayesian inference engine. A Bayesian model is represented as an association list of `(hypothesis . probability)` pairs.

```scheme
;; File: bayes.ss
;; Bayesian inference core.
;;
;; A Bayes model is simply a normalised alist of (hypothesis . probability).
;; UPDATE applies Bayes' Theorem:
;;
;;   P(H | E) = P(E | H) * P(H)  /  sum_h P(E | h) * P(h)

(export make-bayes-model update posterior posteriors maximum-a-posteriori)

;; Sum a list of numbers (fold avoids apply's argument-count limit on big lists).
(def (sum xs) (foldl + 0 xs))

(def (sum-cdrs alist)
  (sum (map cdr alist)))

;; Create a Bayes model from PRIOR-ALIST, an alist of (HYPOTHESIS . PRIOR).
;; Priors are automatically normalised so they sum to 1.
(def (make-bayes-model prior-alist)
  (let ((total (sum-cdrs prior-alist)))
    (when (zero? total)
      (error "All priors are zero -- cannot normalise."))
    (map (lambda (pair) (cons (car pair) (/ (cdr pair) total)))
         prior-alist)))

;; Return a new model with posteriors computed via Bayes' Theorem.
;; EVIDENCE is an arbitrary datum passed to LIKELIHOOD-FN, a function of two
;; arguments (HYPOTHESIS EVIDENCE) that returns P(evidence | hypothesis).
(def (update model evidence likelihood-fn)
  (let* ((unnormalised
          (map (lambda (pair)
                 (cons (car pair)
                       (* (likelihood-fn (car pair) evidence) (cdr pair))))
               model))
         (marginal (sum-cdrs unnormalised)))
    (when (zero? marginal)
      (error "Marginal likelihood is zero -- evidence is impossible under all hypotheses."))
    (map (lambda (pair) (cons (car pair) (/ (cdr pair) marginal)))
         unnormalised)))

;; Return the posterior probability for HYPOTHESIS in MODEL.
(def (posterior model hypothesis)
  (let ((entry (assoc hypothesis model)))
    (if entry
      (cdr entry)
      (error "Hypothesis not found in model:" hypothesis))))

;; Return the full posterior alist.
(def (posteriors model) model)

;; Return (HYPOTHESIS . PROBABILITY) for the most probable hypothesis.
(def (maximum-a-posteriori model)
  (foldl (lambda (pair best) (if (> (cdr pair) (cdr best)) pair best))
         (car model)
         (cdr model)))
```

In `update`, `foldl` is used to compute the marginal likelihood, safely handling arbitrarily large lists without hitting call stack limits.

## Correlation Analysis (`correlation.ss`)

The module `correlation.ss` provides tools to evaluate association between variables, including Pearson product-moment correlation `r`$ and Spearman rank-order correlation `\rho`$.

```scheme
;; File: correlation.ss
;; Correlation helpers.
;;
;; NOTE: Correlation measures *association*, NOT causation.
;; A high |r| between X and Y does not imply that X causes Y,
;; that Y causes X, or even that they share a direct mechanism.

(import :std/sort)
(export mean std-dev rank-list pearson-r spearman-rho correlation-matrix)

;; Sum a list of numbers (fold avoids apply's argument-count limit on big lists).
(def (sum xs) (foldl + 0 xs))

;; Arithmetic mean of a list of numbers.
(def (mean xs)
  (/ (sum xs) (length xs)))

;; Population standard deviation of a list of numbers.
(def (std-dev xs)
  (let ((m (mean xs)))
    (sqrt (/ (sum (map (lambda (x) (expt (- x m) 2)) xs))
             (length xs)))))

;; Return a list of ranks (1-based, average ties) for XS.
(def (rank-list xs)
  (let* ((n (length xs))
         (indexed (let loop ((l xs) (i 0) (acc '()))
                    (if (null? l)
                      (reverse acc)
                      (loop (cdr l) (+ i 1) (cons (cons (car l) i) acc)))))
         (sorted (list->vector (sort indexed (lambda (a b) (< (car a) (car b))))))
         (ranks (make-vector n 0.0)))
    ;; Walk the sorted vector, assigning the average rank to each tie group.
    (let loop ((i 0))
      (when (< i n)
        (let find-end ((j i))
          (if (and (< j n)
                   (= (car (vector-ref sorted j)) (car (vector-ref sorted i))))
            (find-end (+ j 1))
            ;; average of ranks (i+1)..j is (i+1+j)/2
            (let ((avg (/ (+ i 1 j) 2.0)))
              (do ((k i (+ k 1))) ((= k j))
                (vector-set! ranks (cdr (vector-ref sorted k)) avg))
              (loop j))))))
    (vector->list ranks)))

;; Pearson product-moment correlation coefficient.
;; WARNING: measures linear association only -- not causation.
(def (pearson-r xs ys)
  (unless (= (length xs) (length ys))
    (error "XS and YS must have the same length."))
  (let ((n (length xs))
        (mx (mean xs))
        (my (mean ys))
        (sx (std-dev xs))
        (sy (std-dev ys)))
    (if (or (zero? sx) (zero? sy))
      0.0  ; a constant variable has no correlation
      (/ (sum (map (lambda (x y) (* (- x mx) (- y my))) xs ys))
         (* n sx sy)))))

;; Spearman rank-order correlation: Pearson r computed on ranks.
;; WARNING: measures monotonic association only -- not causation.
(def (spearman-rho xs ys)
  (pearson-r (rank-list xs) (rank-list ys)))

;; Given DATA-ALIST of (NAME . VALUES-LIST) pairs, return a list of
;; (NAME-A NAME-B . PEARSON-R) triples for all pairs.
(def (correlation-matrix data-alist)
  (append-map
   (lambda (a)
     (map (lambda (b)
            (cons (car a) (cons (car b) (pearson-r (cdr a) (cdr b)))))
          data-alist))
   data-alist))

(def (append-map f l) (apply append (map f l)))
```

The `rank-list` procedure properly handles tied values by assigning the average rank across all tied elements in the group, ensuring an unbiased Spearman correlation.

## The Frequentist Toolkit (`frequentist.ss`)

The module `frequentist.ss` implements statistical hypothesis tests and probability distributions, including standard normal and chi-squared cumulative distribution functions, z-tests, and Wilson score intervals.

```scheme
;; File: frequentist.ss
;; Frequentist hypothesis-testing toolkit: z-tests, chi-squared tests,
;; and confidence intervals.

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
```

The functions `z-test-proportion`, `chi-squared-test`, `chi-squared-independence`, and `confidence-interval-proportion` return multiple values using Scheme's `values` primitive, unpacked by callers using `let-values`.

## The Medical Screening Example (`medical.ss`)

The module `medical.ss` models the 0.1% prevalence medical screening scenario using Bayesian updating and synthetic population correlation.

```scheme
;; File: medical.ss
;; Medical-diagnosis worked example (Bayesian + correlation).

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
```

## The Frequentist Trial Simulation (`frequentist-demo.ss`)

The module `frequentist-demo.ss` runs a simulated clinical trial of 100,000 screened patients and analyzes the results using frequentist hypothesis tests:

```scheme
;; File: frequentist-demo.ss
;; Re-examines the medical screening scenario from a frequentist standpoint:
;;   1. Simulate a clinical trial (N = 100,000).
;;   2. Chi-squared test of independence between test result and disease.
;;   3. Wilson confidence interval for positive predictive value (PPV).
;;   4. Z-test of the positive-test rate against prevalence.
;;   5. Side-by-side comparison with the Bayesian posterior.

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
```

## Running the Code and Expected Output

Execute the complete probability suite using `make test`:

```console
$ make test
gxi test.ss
=== Sanity checks ===
  [PASS] Phi(0) = 0.5
  [PASS] Phi(1.96) ~ 0.975
  [PASS] Pearson r of perfectly linear data = 1
  [PASS] Spearman rho of monotonic non-linear data = 1
  [PASS] rank-list averages ties
  [PASS] chi-squared of perfect fit = 0

Correlation matrix example:
  r(height, height) = 1.000
  r(height, weight) = 0.997
  r(height, shoe) = 0.000
  r(weight, height) = 0.997
  r(weight, weight) = 1.000
  r(weight, shoe) = 0.043
  r(shoe, height) = 0.000
  r(shoe, weight) = 0.043
  r(shoe, shoe) = 1.000

=== Bayesian Analysis: Medical Screening Test ===
Prior probabilities:
  P(disease) = 0.0010
  P(healthy) = 0.9990

After a POSITIVE test result:
  P(disease | positive) = 0.0194  (1.94%)
  P(healthy | positive) = 0.9806  (98.06%)

MAP hypothesis: healthy

Key insight: despite 99% sensitivity, a positive test
only yields about 1.9% probability of disease because the
disease is so rare (0.1% prevalence). This is exactly
the kind of counter-intuitive result Bayes' Theorem reveals.

=== Correlation Analysis (N = 100,000) ===
Pearson r(test-result, disease) = 0.1439

This positive correlation is real but modest. It shows
that the test result and disease status are associated,
but the correlation coefficient alone cannot tell you the
probability that any *individual* patient is sick -- that
requires Bayesian reasoning with the base rate (prevalence).

=== Done. ===

================================================================
  FREQUENTIST ANALYSIS: Medical Screening Test
================================================================

--- 1. Simulated Clinical Trial (N = 100,000) ---
  True  Positives (TP): 103
  False Positives (FP): 4930
  True  Negatives (TN): 94967
  False Negatives (FN): 0

--- 2. Chi-Squared Test of Independence ---
  chi-squared = 1945.50   df = 1   p-value < 1e-15 (essentially zero)

  Interpretation: the test result and disease status
  are NOT independent (we reject H0). But this only
  means the *association exists* -- it says nothing about
  how strong it is or what it means for one patient.

--- 3. Positive Predictive Value (PPV) ---
  PPV = TP / (TP + FP) = 103 / 5033 = 0.0205  (2.05%)
  95% Wilson CI for PPV: [0.0169, 0.0248]  (1.69% - 2.48%)

--- 4. Z-Test: Positive Rate vs. Prevalence ---
  Observed positive rate: 5.0330%
  Hypothesised rate (prevalence): 0.1000%
  z = 493.5468   p-value < 1e-15 (essentially zero)

  The positive rate far exceeds the disease prevalence
  because of the 5% false-positive rate -- most positives
  are healthy people.

--- 5. Bayesian vs. Frequentist Side-by-Side ---
  Bayesian posterior P(disease | positive test) = 0.0194  (1.94%)
  Frequentist PPV from simulation             = 0.0205  (2.05%)

  Both frameworks agree: a positive test on a rare disease
  gives only about 2% probability of actual illness.

================================================================
  Key lesson: statistical significance (small p-value) and
  practical significance (high PPV) are different things.
================================================================
```

## Interpreting the Results

The side-by-side output illustrates the core statistical lesson:

1. **Alignment Across Paradigms**: The Bayesian posterior `P(\mathrm{disease} \mid \mathrm{positive}) = 1.94\%`$ matches the simulated empirical trial proportion `\mathrm{PPV} = 2.05\%`$ within sampling error. Both methods confirm that approximately 98% of people testing positive are actually healthy.
2. **The p-Value Fallacy**: The chi-squared test of independence yields `\chi^2 = 1945.50`$ with `p < 10^{-15}`$. A scientific paper could truthfully report that the test result is correlated with disease status with extreme statistical significance. Yet for an individual patient, a positive test result only means a 2% chance of disease. Statistical significance detects the existence of a real relationship; it does not measure clinical certainty or high diagnostic utility.
3. **Correlation Bounds**: The Pearson correlation between test result and diagnosis is `r \approx 0.14`$. Even though the test has 99% sensitivity, the low prevalence keeps overall linear correlation modest.

## Wrap Up

In this chapter we built a complete statistical toolkit in Gerbil Scheme covering both Bayesian updating and frequentist hypothesis testing. By modeling medical screening through both lenses, we saw that Bayesian priors and frequentist sampling distributions represent two complementary mathematical views of the same reality. Understanding both paradigms prevents common pitfalls such as the base rate fallacy and the conflation of p-value significance with practical predictive value.

## Optional Practice Problems

1. **Repeated Bayesian Testing**: Use `bayes.ss` to simulate a second independent test on the same patient. Assuming the second test has identical sensitivity and false-positive rates, compute the posterior probability after two consecutive positive test results.
2. **Two-Sample Difference in Proportions**: Implement `(z-test-proportions-two-sample x1 n1 x2 n2)` in `frequentist.ss` using pooled standard error to test whether two independent binomial populations differ significantly.
3. **Bayes Factor Calculator**: Add a function `(bayes-factor model h1 h2 evidence likelihood-fn)` that computes the ratio of posterior odds to prior odds for two competing hypotheses.
