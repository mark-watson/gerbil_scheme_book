(import :nlp/main)

(export main test-text get-categories)

(def (get-categories x)
  (map car (cadddr x)))

(def test-text
#<<TESTTEXT
John Smith, a vice president of Microsoft, went to Mexico to buy a company even though there is a financial collapse.
Food prices are increasing rapidly in many parts of the world.
The UN is concerned about the situation and the risk of war breaking out and other violence
TESTTEXT
)

(def (main)
  (let* ((response (process-string test-text))
         (cats (get-categories response)))
    (displayln "Extracted categories: " cats)
    cats))
