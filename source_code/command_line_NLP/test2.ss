(import :openai/openai) ;; Import NLP library providing process-file

(export main)

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
         (openai/openai#openai "what is 1 + 2?")))
    (displayln response)))

