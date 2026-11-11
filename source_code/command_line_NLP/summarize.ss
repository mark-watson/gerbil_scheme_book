#!/usr/bin/env gxi

(import :openai/openai
        :std/misc/ports)  ;; read-all-as-string

(export main)

(def system-prompt
#<<PTEXT
You are a master at linguistics, technology, and general knowledge. When given input
text you return an accurate summary of the text. You only return the summary,
no other text.
PTEXT
)

(def (main . args)
  (let* ((input-str (if (null? args)
                        (read-all-as-string (current-input-port)) ; piped stdin
                        (string-join args " ")))                  ; argv
         (response (openai input-str system-prompt: system-prompt)))
    (displayln response)))
