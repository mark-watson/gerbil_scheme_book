(import :nlp/main) ;; Import NLP library providing process-file
(import :nlp/nlp)

(export main test-text get-categories)

(def get-categories (lambda (x) (map car (cadddr x)))) ;; helper to extract categories from response

(def test-text
#<<TESTTEXT
John Smith, a vice president of Microoft, went to Mexico to buy a company even though there is a financial collapse.
Food prices are increasing rapidly in many parts of the world.
The UN is concerned about the situation and the risk of war breaking out and other violence
TESTTEXT
)

(def (main)
  (let ((response
         (nlp/main#process-string test-text)))
    (get-categories response)))

