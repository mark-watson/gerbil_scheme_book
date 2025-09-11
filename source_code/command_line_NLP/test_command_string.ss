#!/usr/bin/env gxi

(export main)
(import :std/format) ;; for 'format'

(def (main . args)
  (displayln args)
  (let ((s (string-join args " ")))
    (displayln s)))
