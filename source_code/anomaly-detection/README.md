# Gaussian Anomaly Detection

An implementation of a Gaussian Anomaly Detection engine from scratch in Gerbil Scheme. This project is ported from a TypeScript equivalent.

## Overview

The engine fits a multivariate Gaussian distribution with a diagonal covariance matrix (meaning feature dimensions are assumed independent). It uses:
- **Mean ($\mu$)** and **Variance ($\sigma^2$)** calculated feature-by-feature on the normal training examples.
- **Gaussian PDF** to compute the likelihood $p(x)$ for cross-validation examples.
- **Epsilon ($\epsilon$) sweep** to find the optimal classification threshold that maximizes correct categorization on the cross-validation set.
- **Test set** evaluation to output precision, recall, and F1-score.

## Dataset

The demo uses the **Wisconsin Breast Cancer Dataset** (`data/cleaned_wisconsin_cancer_data.csv`).
- **Features:** 9 cytological features of breast fine needle aspirate (FNA) samples.
- **Target:** Class (2 for benign, 4 for malignant/anomaly).
- **Preprocessing:** Scales values by 0.1, applies a log-transform `log(x + 1.2)`, scales values between 0.0 and 1.0, and maps target classes to `{0.0, 1.0}`.

## Files

| File | Description |
|------|-------------|
| `detector.ss` | Anomaly detection engine (fitting parameters, evaluating PDF, and model testing) |
| `wisconsin_demo.ss` | Dataset loader, preprocessing logic, training, and evaluations |
| `data/cleaned_wisconsin_cancer_data.csv` | Wisconsin breast cancer dataset |
| `Makefile` | Simple target for running the tests |
| `gerbil.pkg` | Package declaration namespace (`anomaly-detection`) |

## Running the Demo

Run `make test` to execute the demo program:

```bash
make test
# or equivalently:
gxi wisconsin_demo.ss
```
