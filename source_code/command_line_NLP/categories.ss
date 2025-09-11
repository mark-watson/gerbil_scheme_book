#!/usr/bin/env gxi

(import :nlp/main)
(import :nlp/nlp)
(import :std/iter)

(export main)

(def get-categories (lambda (x) (map car (cadddr x)))) ;; helper to extract categories from response

(def (remove-txt str substr)
  (if (string-suffix? substr str)
      (substring str 0 (- (string-length str) (string-length substr)))
      str))

(def (main . args)
  (let ((response
         (nlp/main#process-string (string-join args " "))))
    (for (s (get-categories response))
      (displayln (remove-txt s ".txt")))))