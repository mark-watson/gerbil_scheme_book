;; File: test.ss
;; Non-interactive smoke test: runs 2 simulated turns with the Ollama Game Master.

(import :std/format
        "game")

(def (run-test)
  (displayln "=== Testing Text Adventure Game with Ollama ===")
  (let* ((story (read-file-string "story.txt"))
         (messages `(((role . "system") (content . ,story))
                     ((role . "user") (content . "Begin the game.")))))
    (displayln "\n[Turn 1: Opening scene request]")
    (let ((opening (chat-ollama messages model: "qwen3.5:4b")))
      (displayln opening)
      (let* ((messages2 (append messages
                               `(((role . "assistant") (content . ,opening))
                                 ((role . "user") (content . "I approach the stream and look into the water."))))))
        (displayln "\n[Turn 2: Player action -> 'look into stream']")
        (let ((turn2 (chat-ollama messages2 model: "qwen3.5:4b")))
          (displayln turn2)
          (displayln "\n=== Smoke Test Passed: Game Master is responsive ==="))))))

(run-test)
