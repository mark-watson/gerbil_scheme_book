#!/usr/bin/env gxi

(import :openai/openai
        :std/iter
        :std/misc/ports)  ;; read-all-as-string

(export main)

(def system-prompt
#<<PTEXT
You ar a master at libguistics, technology, and general knowledge. When given input
text you return an accurate summary of the text. You only return the summary,
no other text.

HERE IS THE TEXT TO SUMMARIZE:

PTEXT
)


(def (main . args)
  (let* ((input-str (if (null? args)
                        (read-all-as-string (current-input-port)) ; piped stdin
                        (string-join args " ")))                  ; argv
         (response (openai/openai#openai (string-append input-str system-prompt))))
    (displayln response)))
