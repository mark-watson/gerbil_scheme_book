#!/usr/bin/env gxi

(import :nlp/main
        :nlp/nlp
        :std/iter
        :std/misc/ports)  ;; read-all-as-string

(export main)

(def get-categories (lambda (x) (map car (cadddr x))))

(def (remove-txt str substr)
  (if (string-suffix? substr str)
      (substring str 0 (- (string-length str) (string-length substr)))
      str))

(def (main . args)
  (let* ((input-str (if (null? args)
                        (read-all-as-string (current-input-port)) ; piped stdin
                        (string-join args " ")))                  ; argv
         (response (nlp/main#process-string input-str)))
    (for (s (get-categories response))
      (displayln (remove-txt s ".txt"))))
)