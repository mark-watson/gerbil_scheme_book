# Probability: Bayesian and Frequentist Reasoning

A small Gerbil Scheme library for exploring the two major schools of statistical reasoning, ported from the Common Lisp `Probability` example in *Loving Common Lisp*. It needs only the Gerbil standard library.

The main worked example is a medical screening test. A rare disease (0.1% prevalence) is screened with a test that has 99% sensitivity and a 5% false-positive rate. If a patient tests positive, the chance they actually have the disease is only about **1.9%**. The demo gets this result from Bayes' Theorem, then reaches the same conclusion from a simulated frequentist trial.

## Files

| File | Description |
|------|-------------|
| `bayes.ss` | Bayesian inference: priors, updates, posteriors, MAP |
| `correlation.ss` | Pearson r, Spearman rho, correlation matrix |
| `frequentist.ss` | Normal/chi-squared CDF approximations, z-tests, chi-squared tests, Wilson CI |
| `medical.ss` | Bayesian and correlation analysis of the screening test |
| `frequentist-demo.ss` | Frequentist analysis of the same scenario, compared with the Bayesian result |
| `fmt.ss` | Fixed-precision number formatting helpers |
| `test.ss` | Sanity checks plus both demos |

## Running

```bash
make test       # or: gxi test.ss
make build      # compile all modules with gxc
```

## API

### Bayesian (`bayes.ss`)

- `(make-bayes-model prior-alist)`: builds a model from `((hypothesis . prior) ...)`. Priors are normalised so they sum to 1.
- `(update model evidence likelihood-fn)`: applies Bayes' Theorem and returns a new model. `likelihood-fn` is `(lambda (hypothesis evidence) ...)` and returns P(evidence | hypothesis).
- `(posterior model hypothesis)`, `(posteriors model)`, `(maximum-a-posteriori model)`

```scheme
(import "bayes")
(def prior (make-bayes-model '((disease . 0.001) (healthy . 0.999))))
(def post (update prior 'positive
                  (lambda (h e) (if (eq? h 'disease) 0.99 0.05))))
(posterior post 'disease)   ; => 0.0194...
```

### Correlation (`correlation.ss`)

- `(pearson-r xs ys)`, `(spearman-rho xs ys)`, `(rank-list xs)`
- `(correlation-matrix '((name . values) ...))` returns `(name-a name-b . r)` triples.

Correlation measures association, **not** causation.

### Frequentist (`frequentist.ss`)

Functions that return several results use Scheme multiple values, so read them with `let-values`:

- `(z-test-proportion successes n p0)` returns `z` and `p-value`
- `(chi-squared-test observed expected)` returns `chi2`, `df` and `p-value`
- `(chi-squared-independence table)` returns `chi2`, `df` and `p-value`
- `(confidence-interval-proportion successes n confidence: 0.95)` returns `lower` and `upper` (Wilson score interval)
- `(phi-approx z)`, `(chi-squared-cdf x df)`, `(z-critical confidence)`

## Porting Notes (Common Lisp to Gerbil)

- CL `(values ...)` / `multiple-value-bind` became Scheme `values` / `let-values`.
- CL `&key` arguments became Gerbil keyword arguments (`confidence: (confidence 0.95)`).
- Gerbil's `:std/format` has no `~,4F` precision directive, so `fmt.ss` provides `fixed` and `percent`.
- Sums use `(foldl + 0 xs)` rather than `(apply + xs)`. Gambit limits how many arguments a function call can take, and the 100,000-element simulation lists go over that limit.
