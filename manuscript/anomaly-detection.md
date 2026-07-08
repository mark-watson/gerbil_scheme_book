# Gaussian Anomaly Detection

Anomaly detection is one of the most practical applications of unsupervised and semi-supervised machine learning. Rather than asking "what class does this belong to?", we ask a subtler and often more useful question: "does this observation look like the ones I have already seen?" In fraud detection, manufacturing quality control, medical diagnostics, network intrusion monitoring, and predictive maintenance, the interesting events are usually rare. Trying to build a supervised classifier for events that occur one time in a thousand often fails - there are simply not enough positive examples to learn a good decision boundary. Anomaly detection sidesteps this problem entirely by modeling the *normal* distribution of data and flagging observations that fall outside it.

In this chapter we implement a **multivariate Gaussian anomaly detector** in Gerbil Scheme from scratch, then apply it to the classic Wisconsin Breast Cancer dataset. The detector fits a Gaussian distribution to each feature independently (a "diagonal covariance" assumption), computes the probability density `p(x)`$ of each observation, and flags observations whose density falls below a threshold `\epsilon`$ as anomalies. The threshold is selected automatically by sweeping over 200 candidate values and picking the one that maximizes performance on a cross-validation set.

## Theoretical Background

The core idea is that if we assume each feature `x_j`$ of a "normal" observation is drawn from a Gaussian distribution with mean `\mu_j`$ and variance `\sigma^2_j`$, then the probability density of a full observation vector `x = (x_1, x_2, \dots, x_n)`$ can be approximated by combining the per-feature Gaussians. The Gaussian probability density function for a single feature is:

```$
p(x_j; \mu_j, \sigma^2_j) = \frac{1}{\sqrt{2\pi\sigma^2_j}} \exp\left(-\frac{(x_j - \mu_j)^2}{2\sigma^2_j}\right)
```

A standard multivariate Gaussian anomaly detector multiplies these per-feature densities together (assuming feature independence). The implementation in this chapter uses a slight variation: it computes the **arithmetic mean** of the per-feature PDFs rather than their product. This has the practical benefit of keeping the resulting numbers on a comfortable scale (avoiding vanishingly small products when there are many features) while still giving a monotone score suitable for thresholding.

The training procedure is straightforward:

1. **Split** the examples into training (~60%), cross-validation (~20%), and test (~20%) sets. The training set is filtered to contain mostly normal examples.
2. **Fit** the per-feature mean `\mu_j`$ and variance `\sigma^2_j`$ using only the training set.
3. **Sweep** many candidate values of `\epsilon`$ and pick the one that yields the fewest misclassifications on the cross-validation set.
4. **Evaluate** the final model on the held-out test set, reporting precision, recall, and `F_1`$-score.

A key trick used here (following Andrew Ng's classic Coursera Machine Learning lectures) is that we deliberately let a small number of anomalous examples leak into the CV and test sets - they are essential for tuning `\epsilon`$ and for measuring recall.

## The Dataset

We use the **Wisconsin Breast Cancer Dataset**, a well-known benchmark in medical machine learning. Each row is derived from a fine needle aspirate (FNA) of a breast mass and contains 9 cytological features scored on an integer scale from 1 to 10, plus a target label:

- Features (columns 1-9): clump thickness, uniformity of cell size, uniformity of cell shape, marginal adhesion, single epithelial cell size, bare nuclei, bland chromatin, normal nucleoli, mitoses.
- Target (column 10): `2` for benign, `4` for malignant.

The raw CSV format is refreshingly simple. Here are the first few lines of `data/cleaned_wisconsin_cancer_data.csv`:

```
10,10,10,8,6,1,8,9,1,4
6,2,1,1,1,1,7,1,1,2
5,4,4,9,2,10,5,6,1,4
2,5,3,3,6,7,7,5,1,4
10,4,3,1,3,3,6,5,2,4
6,10,10,2,8,10,7,3,3,4
5,6,5,6,10,1,3,1,1,4
10,10,10,4,8,1,8,10,1,4
1,1,1,1,2,1,2,1,2,2
```

The final column of `2` or `4` is what we want to remap to `{0.0, 1.0}` so the detector can distinguish anomalies (malignant) from normals (benign).

## Project Structure

The project directory `source_code/anomaly-detection` contains:

| File | Description |
|------|-------------|
| `detector.ss` | The anomaly detection engine: parameter fitting, PDF evaluation, threshold sweep, and model testing. |
| `wisconsin_demo.ss` | CSV loading, preprocessing, training, and end-to-end evaluation on the Wisconsin dataset. |
| `data/cleaned_wisconsin_cancer_data.csv` | The Wisconsin breast cancer dataset (648 examples). |
| `Makefile` | Single `test` target that invokes `gxi wisconsin_demo.ss`. |
| `gerbil.pkg` | Package declaration (`anomaly-detection`). |

## The Detector Module: detector.ss

This module contains the entire anomaly detection engine. It has no dependency on the Wisconsin dataset - it can be reused for any labelled dataset in which the final column encodes the anomaly label as `1.0` (anomaly) or `0.0` (normal).

We start with the imports and exports. The `:std/format` module gives us `format` for printing, `:std/pregexp` is available for regex use in downstream modules, and `:gerbil/gambit` exposes the underlying Gambit runtime primitives.

```scheme
;; File: detector.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

(export split-data
        compute-mu
        compute-sigma-sq
        gaussian-p
        build-detector
        train
        is-anomaly
        test-model
        format-decimal)

(def SQRT_2_PI (sqrt (* 2.0 (acos -1.0))))
```

The constant `SQRT_2_PI` is precomputed once as `\sqrt{2\pi}`$ because it appears in the denominator of every Gaussian PDF evaluation. Note the small trick `(acos -1.0)` to obtain `\pi`$ without hardcoding a magic number.

### Decimal Formatting Helpers

Gerbil's default number printing yields long decimal expansions (like `0.8571428571428571`), which are inconvenient for reporting metrics. The next two helpers give us `format-decimal` for pretty-printing floats to a fixed number of places:

```scheme
;; Find index of character in string (helper for format-decimal)
(def (string-index str char)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) char) i)
        (else (loop (+ i 1)))))))

;; Format floating point numbers to a fixed number of decimal places
(def (format-decimal x places)
  (let* ((multiplier (expt 10 places))
         (rounded (/ (round (* x multiplier)) multiplier))
         (str (number->string rounded)))
    (let ((dot-pos (string-index str #\.)))
      (let ((formatted-str
             (if dot-pos
               (let* ((frac-len (- (string-length str) dot-pos 1))
                      (diff (- places frac-len)))
                 (if (> diff 0)
                   (string-append str (make-string diff #\0))
                   str))
               (string-append str "." (make-string places #\0)))))
        (if (char=? (string-ref formatted-str 0) #\.)
          (string-append "0" formatted-str)
          formatted-str)))))
```

`format-decimal` multiplies by `10^{\text{places}}`$, rounds to the nearest integer, divides back, then post-processes the string to zero-pad the fractional part and prepend a leading `0` when necessary. The result is a stable, readable representation like `"0.8571"`.

### Splitting the Data

The `split-data` procedure randomly divides the examples into three sets. Crucially, it applies an additional filter to the training set: it accepts an example into training only if it is a normal one (target < 0.5), or with a small 10% probability if it is an anomaly. This mirrors the standard anomaly detection recipe - train primarily on normals, but keep some anomalies in CV and test for threshold tuning and evaluation.

```scheme
;; Split examples into training (~60%), cross-validation (~20%), and test sets (~20%)
(def (split-data examples num-features)
  (let ((train '())
        (cv '())
        (test '())
        (oi (- num-features 1)))
    (for-each
      (lambda (ex)
        (if (< (random-real) 0.6)
          (when (or (< (vector-ref ex oi) 0.5) (< (random-real) 0.1))
            (set! train (cons ex train)))
          (if (< (random-real) 0.7)
            (set! cv (cons ex cv))
            (set! test (cons ex test)))))
      examples)
    `((train . ,(reverse train))
      (cv . ,(reverse cv))
      (test . ,(reverse test)))))
```

The function returns an association list keyed by `'train`, `'cv`, and `'test`. Each example is a vector where index `(- num-features 1)` holds the target label.

### Fitting Mean and Variance

Fitting is textbook: compute the sample mean and sample variance for each feature. `compute-mu` loops over all examples, accumulating a per-feature sum into a vector, then divides by the number of examples:

```scheme
;; Per-feature mean mu
(def (compute-mu examples nf)
  (let* ((mu (make-vector nf 0.0))
         (len (length examples)))
    (if (= len 0)
      mu
      (begin
        (for-each
          (lambda (ex)
            (do ((f 0 (+ f 1)))
                ((= f nf))
              (vector-set! mu f (+ (vector-ref mu f) (vector-ref ex f)))))
          examples)
        (do ((f 0 (+ f 1)))
            ((= f nf) mu)
          (vector-set! mu f (/ (vector-ref mu f) len)))))))
```

`compute-sigma-sq` follows the same shape but accumulates squared deviations from the mean. Note the small floor of `1.0e-10` to prevent divide-by-zero when a feature happens to be perfectly constant on the training set:

```scheme
;; Per-feature variance sigmaSq
(def (compute-sigma-sq examples mu nf)
  (let* ((s2 (make-vector nf 0.0))
         (len (length examples)))
    (do ((f 0 (+ f 1)))
        ((= f (- nf 1)) s2)
      (let ((sum-val 0.0))
        (for-each
          (lambda (ex)
            (let* ((d (- (vector-ref ex f) (vector-ref mu f))))
              (set! sum-val (+ sum-val (* d d)))))
          examples)
        (vector-set! s2 f (max (/ sum-val len) 1.0e-10))))))
```

Notice that the outer loop terminates at `(- nf 1)` rather than `nf`. This deliberately skips the last column (the target label) - we don't want to include the label in the Gaussian model.

### The Gaussian Score

The `gaussian-p` function computes the density score used for classification. As mentioned above, it takes the **arithmetic mean** of the per-feature Gaussian PDFs rather than their product. This is the score we will threshold with `\epsilon`$:

```scheme
;; Average Gaussian PDF p(x) across all features
(def (gaussian-p x mu sigma-sq nf)
  (let ((sum-val 0.0))
    (do ((f 0 (+ f 1)))
        ((= f (- nf 1)) (/ sum-val nf))
      (let* ((s2 (vector-ref sigma-sq f))
             (d (- (vector-ref x f) (vector-ref mu f)))
             (pdf (* (/ 1.0 (* SQRT_2_PI (sqrt s2))) (exp (- (/ (* d d) (* 2.0 s2)))))))
        (set! sum-val (+ sum-val pdf))))))
```

### Assembling the Detector

`build-detector` is the constructor. It takes raw labelled examples, splits them, computes `\mu`$, and stashes everything into a hash table that carries the model state:

```scheme
;; Build a detector from labelled examples
(def (build-detector examples num-features)
  (let* ((split (split-data examples num-features))
         (train (cdr (assoc 'train split)))
         (cv (cdr (assoc 'cv split)))
         (test (cdr (assoc 'test split)))
         (mu (compute-mu train num-features))
         (sigma-sq (make-vector num-features 0.0))
         (ht (make-hash-table)))
    (hash-put! ht 'num-features num-features)
    (hash-put! ht 'mu mu)
    (hash-put! ht 'sigma-sq sigma-sq)
    (hash-put! ht 'best-eps 0.02)
    (hash-put! ht 'training train)
    (hash-put! ht 'cross-validation cv)
    (hash-put! ht 'testing test)
    ht))
```

### The Threshold Sweep

The core of training is to try many candidate values of `\epsilon`$ and pick the one that produces the fewest classification errors on the cross-validation set. `train-helper` recomputes `\sigma^2`$ (from the training set) and counts CV errors for a given epsilon:

```scheme
(def (train-helper det epsilon)
  (let* ((nf (hash-ref det 'num-features))
         (training (hash-ref det 'training))
         (mu (hash-ref det 'mu))
         (cv (hash-ref det 'cross-validation))
         (sigma-sq (compute-sigma-sq training mu nf))
         (errors 0))
    (hash-put! det 'sigma-sq sigma-sq)
    (for-each
      (lambda (x)
        (let* ((prob (gaussian-p x mu sigma-sq nf))
               (anomaly (> (vector-ref x (- nf 1)) 0.5)))
          (if (if anomaly (> prob epsilon) (< prob epsilon))
            (set! errors (+ errors 1))
            #f)))
      cv)
    errors))
```

The subtle bit is the inner `if`: for a true anomaly, an error occurs when the score is *above* `\epsilon`$ (we called it normal); for a true normal, an error occurs when the score is *below* `\epsilon`$ (we called it anomalous).

### Test-Set Evaluation

`test-model` runs the full confusion matrix on the held-out test set and prints precision, recall, and `F_1`$:

```scheme
;; Evaluate model on test data
(def (test-model det epsilon)
  (let* ((nf (hash-ref det 'num-features))
         (mu (hash-ref det 'mu))
         (sigma-sq (hash-ref det 'sigma-sq))
         (testing (hash-ref det 'testing))
         (tp 0) (fp 0) (fn 0) (tn 0))
    (for-each
      (lambda (x)
        (let* ((prob (gaussian-p x mu sigma-sq nf))
               (anomaly (> (vector-ref x (- nf 1)) 0.5)))
          (if anomaly
            (if (> prob epsilon) (set! fn (+ fn 1)) (set! tp (+ tp 1)))
            (if (< prob epsilon) (set! fp (+ fp 1)) (set! tn (+ tn 1))))))
      testing)
    (let* ((precision (if (= (+ tp fp) 0) 0.0 (/ (exact->inexact tp) (+ tp fp))))
           (recall (if (= (+ tp fn) 0) 0.0 (/ (exact->inexact tp) (+ tp fn))))
           (f1 (if (= (+ precision recall) 0.0) 0.0 (/ (* 2.0 precision recall) (+ precision recall)))))
      (displayln (format "\n -- best epsilon = ~a" (format-decimal epsilon 4)))
      (displayln (format " -- test examples  = ~a" (length testing)))
      (displayln (format " -- TP=~a FP=~a FN=~a TN=~a" tp fp fn tn))
      (displayln (format " -- precision=~a recall=~a F1=~a"
                         (format-decimal precision 4)
                         (format-decimal recall 4)
                         (format-decimal f1 4)))
      `((precision . ,precision)
        (recall . ,recall)
        (f1 . ,f1)))))
```

### The Top-Level Training Loop and Anomaly Predicate

Finally, `train` sweeps 200 values of `\epsilon`$ starting from `0.001` in increments of `0.005`, keeps the value that minimized CV errors, then evaluates on the test set:

```scheme
;; Sweep 200 epsilon values, pick best, evaluate on test set
(def (train det)
  (let ((best-err 1.0e10)
        (best-e 0.001))
    (do ((i 0 (+ i 1)))
        ((= i 200))
      (let* ((eps (+ 0.001 (* 0.005 i)))
             (err (train-helper det eps)))
        (when (<= err best-err)
          (set! best-err err)
          (set! best-e eps))))
    (displayln (format "\n**** Best epsilon = ~a" (format-decimal best-e 4)))
    (hash-put! det 'best-eps best-e)
    (train-helper det best-e)
    (test-model det best-e)
    det))

;; Return true if x is classified as an anomaly
(def (is-anomaly det x)
  (< (gaussian-p x (hash-ref det 'mu) (hash-ref det 'sigma-sq) (hash-ref det 'num-features))
     (hash-ref det 'best-eps)))
```

`is-anomaly` is the user-facing predicate: given a trained detector and a new observation vector, return `#t` if the Gaussian score falls below the learned threshold.

## The Demo Program: wisconsin_demo.ss

The demo file glues the detector to the Wisconsin dataset. It imports the detector module (via the sibling filename import `"detector"`) plus a few standard modules for I/O and regex splitting:

```scheme
;; File: wisconsin_demo.ss
(import "detector")
(import :std/misc/ports
        :std/pregexp
        :std/format
        :gerbil/gambit)
```

### Loading CSV Data

`load-csv` reads the file line-by-line, trims whitespace, splits on commas, converts each token to a number, and returns a list of vectors:

```scheme
;; Trim leading and trailing whitespace
(def (string-trim str)
  (pregexp-replace "^\\s+" (pregexp-replace "\\s+$" str "") ""))

(def (load-csv path)
  (let ((lines (read-file-lines path)))
    (let loop ((lines lines) (acc '()))
      (if (null? lines)
        (reverse acc)
        (let* ((line (string-trim (car lines)))
               (trimmed-len (string-length line)))
          (if (> trimmed-len 0)
            (let* ((tokens (pregexp-split "," line))
                   (numbers (map string->number tokens))
                   (vec (list->vector numbers)))
              (loop (cdr lines) (cons vec acc)))
            (loop (cdr lines) acc)))))))
```

Empty lines are silently skipped, which makes the loader forgiving of trailing newlines.

### Preprocessing the Wisconsin Rows

Raw integer scores from 1 to 10 are not ideal for Gaussian modeling - the features are heavily skewed, with many observations pinned at `1`. The preprocessing pipeline in `preprocess-wisconsin` applies three transformations to each row:

1. **Scale down** the raw features (indices 0-8) by 0.1, moving them into a smaller numeric range.
2. **Log-transform** each feature as `\log(x + 1.2)`$. The additive constant prevents `\log(0)`$ and softens the tail.
3. **Per-row min-max scale** the features to the interval `[0, 1]`$, so no feature dominates the Gaussian by virtue of its raw magnitude.

The target column is separately remapped from `\{2.0, 4.0\}`$ to `\{0.0, 1.0\}`$:

```scheme
;; Log-transform, per-row min-max scaling, remap target {2,4} -> {0,1}
(def (preprocess-wisconsin rows)
  (map
    (lambda (row)
      (let* ((xs (make-vector 10 0.0))
             (mn 1.0e9)
             (mx -1.0e9))
        ;; Multiply feature values (indices 0 to 8) by 0.1
        (do ((i 0 (+ i 1)))
            ((= i 9))
          (vector-set! xs i (* (vector-ref row i) 0.1)))
        ;; Copy target (index 9)
        (vector-set! xs 9 (vector-ref row 9))

        ;; Compute log and find min/max
        (do ((i 0 (+ i 1)))
            ((= i 9))
          (let ((v (log (+ (vector-ref xs i) 1.2))))
            (vector-set! xs i v)
            (when (< v mn) (set! mn v))
            (when (> v mx) (set! mx v))))

        ;; Min-max scale the features
        (let ((range (- mx mn)))
          (do ((i 0 (+ i 1)))
              ((= i 9))
            (if (< range 1.0e-10)
              (vector-set! xs i 0.5)
              (vector-set! xs i (/ (- (vector-ref xs i) mn) range)))))

        ;; Normalize target from {2.0, 4.0} to {0.0, 1.0}
        (vector-set! xs 9 (* (- (vector-ref xs 9) 2.0) 0.5))
        xs))
    rows))
```

### The Main Entry Point

The `main` function ties everything together: load the CSV, preprocess, build the detector, train it (which internally sweeps `\epsilon`$ and evaluates on test), then run a couple of light sanity checks:

```scheme
(def (main)
  (let* ((data (preprocess-wisconsin (load-csv "data/cleaned_wisconsin_cancer_data.csv"))))
    (displayln (format "Loaded ~a examples." (length data)))

    (let* ((det (build-detector data 10)))
      (displayln (format "\nTraining set:  ~a" (length (hash-ref det 'training))))
      (displayln (format "Cross-val set: ~a" (length (hash-ref det 'cross-validation))))
      (displayln (format "Test set:      ~a" (length (hash-ref det 'testing))))

      (train det)

      (displayln (format "\nModel: bestEps=~a, features=~a"
                         (format-decimal (hash-ref det 'best-eps) 4)
                         (hash-ref det 'num-features)))

      ;; Assertions
      (unless (> (hash-ref det 'best-eps) 0.0)
        (error "epsilon should be positive"))
      (unless (= (vector-length (hash-ref det 'mu)) 10)
        (error "mu should have 10 elements"))

      (let ((testing (hash-ref det 'testing)))
        (when (> (length testing) 0)
          (let* ((s (car testing))
                 (actual (if (> (vector-ref s 9) 0.5) "anomaly" "normal"))
                 (predicted (if (is-anomaly det s) "anomaly" "normal")))
            (displayln (format "\nFirst test sample: actual=~a, predicted=~a" actual predicted)))))

      (displayln "All assertions passed.")
      (displayln "=== Test complete ==="))))

(main)
```

## Running the Demo

Run either `make test` or invoke the interpreter directly. The Makefile is minimal:

```makefile
test:
	gxi wisconsin_demo.ss
```

A typical run produces output similar to the following. Because `split-data` uses random sampling, the exact numbers will vary from run to run:

```console
$ make test
gxi wisconsin_demo.ss
Loaded 648 examples.

Training set:  259
Cross-val set: 188
Test set:      68

**** Best epsilon = 0.8910

 -- best epsilon = 0.8910
 -- test examples  = 68
 -- TP=24 FP=4 FN=1 TN=39
 -- precision=0.8571 recall=0.9600 F1=0.9057

Model: bestEps=0.8910, features=10

First test sample: actual=anomaly, predicted=anomaly
All assertions passed.
=== Test complete ===
```

## Interpreting the Results

The output reports several important quantities.

- **Loaded 648 examples**: the total number of records read from the CSV.
- **Training / Cross-val / Test set sizes**: notice that the training set is smaller than a naive 60% split would give. That is expected - `split-data` deliberately drops most of the anomalous rows from the training set so the model learns what "normal" looks like.
- **Best epsilon = 0.8910**: the classification threshold selected by the sweep. Any observation whose Gaussian score falls below `0.8910` is flagged as an anomaly.
- **TP=24 FP=4 FN=1 TN=39**: the confusion matrix on the 68-observation test set. `TP` (true positive) is a correctly-identified malignant case, `FN` (false negative) is a missed malignant case, `FP` (false positive) is a benign case wrongly flagged, and `TN` (true negative) is a correctly-identified benign case.
- **precision = 0.8571**: of the 28 observations the model called "anomaly", 24 were truly malignant. Precision answers "when the model raises an alarm, how often is it right?"
- **recall = 0.9600**: of the 25 truly malignant observations, the model caught 24. Recall answers "of all the actual anomalies, how many did we catch?"
- **`F_1`$ = 0.9057**: the harmonic mean of precision and recall. `F_1`$ is a single-number summary of overall detection quality when both false positives and false negatives matter.

The recall value is particularly encouraging in a medical setting - missing a malignant tumor is a much more costly mistake than a false alarm, so a recall above 0.95 is a genuinely useful outcome.

## Wrap Up

We built a full-featured Gaussian anomaly detection engine in Gerbil Scheme in fewer than 200 lines of code. The key ideas transfer to any application where "the interesting event is rare":

- Fit a Gaussian per feature on your normal data.
- Use the density function as a score.
- Use a held-out cross-validation set with a few real anomalies in it to pick the best classification threshold `\epsilon`$.
- Report precision, recall, and `F_1`$ on a completely held-out test set.

Gerbil Scheme turns out to be a comfortable environment for this kind of numerical work. Vectors give us cheap random-access storage, hash tables give us a natural home for the trained model's state, and the top-level `def` / `import` / `export` module system keeps `detector.ss` cleanly reusable across different datasets. Try pointing `wisconsin_demo.ss` at your own CSV data - as long as the last column is a binary anomaly label, the detector will happily consume it.

## Optional Practice Problems

1. **Product-of-PDFs Score**: `gaussian-p` currently returns the arithmetic mean of the per-feature PDFs. Add a new function `gaussian-p-product` that instead returns the classical multivariate Gaussian score (the product of the per-feature PDFs) and compare its precision/recall/F1 against the mean-based score on the Wisconsin dataset.

2. **Sensitivity to the Preprocessing Pipeline**: In `preprocess-wisconsin`, remove the log-transform step (leave only the `*0.1` scaling and the min-max normalization). Re-run the demo and report how the best `\epsilon`$, precision, recall, and `F_1`$ change. Explain why the log-transform helps or hurts.

3. **Configurable Sweep Range**: Modify `train` in `detector.ss` to accept optional keyword arguments `eps-start`, `eps-step`, and `eps-count` so users can control the epsilon sweep without editing the source. Provide sensible defaults matching the current behavior (0.001, 0.005, 200).

4. **Save and Load a Trained Detector**: Implement `save-detector` and `load-detector` functions in `detector.ss` that serialize the trained `\mu`$, `\sigma^2`$, `best-eps`, and `num-features` to a plain-text file and read them back in a later session, so that training does not need to be repeated to classify new observations.

5. **Try a Different Dataset**: The KDD Cup 1999 network intrusion dataset is a classic benchmark for anomaly detection. Write a `kdd_demo.ss` that mimics `wisconsin_demo.ss` for a subset of the KDD data, including any needed preprocessing (categorical features must be converted to numeric form). Report how well the detector performs.
