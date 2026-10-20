;; File: test.ss
;; Demo for AutoContext: loads data/*.txt, retrieves context for a query,
;; and builds the prompt. If Ollama is available, optionally sends it.

(import :std/format
        "autocontext")

(def (run-test)
  (let* ((data-dir "data")
         (ac (make-auto-context data-dir))
         (query "who says that economics is bullshit?")
         (prompt (get-prompt ac query num-results: 2)))
    (displayln "\n=== Generated Prompt for LLM ===")
    (displayln prompt)

    ;; Test with Ollama if running
    (with-catch
     (lambda (e)
       (displayln "\n[Note: Local Ollama server not reachable or request failed, skipping LLM query]")
       (displayln "Prompt generation succeeded."))
     (lambda ()
       (displayln "\n=== Querying Ollama (qwen3.5:4b) ===")
       (let ((ans (ask-with-context ac query model: "qwen3.5:4b" num-results: 2)))
         (displayln "\n--- Answer from Ollama ---")
         (displayln ans))))))

(run-test)
