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
