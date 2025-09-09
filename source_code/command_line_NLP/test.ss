(import :nlp/main) ;; Import NLP library providing process-file

(define (main)
  (displayln "Processing string....")
  (displayln (nlp/main#process-string "John Smith went to Mexico to buy a company"))
  (displayln "...done."))
