;;; use_tools.ss — Ollama chat completion with tool/function calling
;;; Ported from the Common Lisp ollama-tools.lisp + ollama-helper.lisp.
;;;
;;; Uses the /api/chat endpoint (required for tool calling) instead of
;;; /api/generate used by ollama.ss.
;;;
;;; Usage (in gxi):
;;;   (import "example_tools" "use_tools")
;;;   (ollama-with-tools "What is the weather in Paris?"
;;;                      default-tool-registry)

(import :std/net/request
        :std/text/json
        :std/srfi/13
        "example_tools")

(export ollama-with-tools
        ollama-chat)

;;; ----------------------------------------------------------------
;;; Model configuration
;;; ----------------------------------------------------------------

(def *tool-model* "qwen3:1.7b")   ;; good small model for tool calling

;; Determine base endpoint from OLLAMA_HOST or default to localhost:11434
(def (ollama-host)
  (let ((host (or (getenv "OLLAMA_HOST" #f) "http://localhost:11434")))
    (if (string-prefix? "http" host)
        host
        (string-append "http://" host))))

;;; ----------------------------------------------------------------
;;; ollama-chat — single chat request (with optional tools)
;;; ----------------------------------------------------------------

;; Send a chat request to Ollama's /api/chat endpoint.
;; MESSAGES: list of hash tables, each with 'role' and 'content' keys.
;; TOOLS: optional list of tool schemas (as produced by tool-registry-schemas).
;; MODEL: model name string.
;; Returns the parsed JSON response as a hash table.
(def (ollama-chat messages
                  tools: (tools #f)
                  model: (model *tool-model*))
  (let* ((endpoint (string-append (ollama-host) "/api/chat"))
         (headers '(("Content-Type" . "application/json")))
         (body-data
           (list->hash-table
             (append
               `(("model" . ,model)
                 ("stream" . #f)
                 ("messages" . ,messages))
               (if tools `(("tools" . ,tools)) '()))))
         (body-string (json-object->string body-data)))
    (with-catch
      (lambda (e)
        (error "Could not connect to Ollama. Is the Ollama server running?" irritant: e))
      (lambda ()
        (let ((response (http-post endpoint headers: headers data: body-string)))
          (if (= (request-status response) 200)
              (request-json response)
              (error "Ollama chat API request failed"
                     status: (request-status response)
                     body: (request-text response))))))))

;;; ----------------------------------------------------------------
;;; Tool dispatch
;;; ----------------------------------------------------------------

;; Execute a single tool call using handlers from REGISTRY.
;; TOOL-CALL is a hash table with 'function' containing 'name' and 'arguments'.
;; Returns the result string.
(def (dispatch-tool-call registry tool-call)
  (let* ((func-info (hash-ref tool-call 'function))
         (name (hash-ref func-info 'name))
         (args (hash-ref func-info 'arguments))
         (tool-record (tool-registry-lookup registry name)))
    (displayln "  [tool-call] " name " args: " args)
    (if tool-record
        (let ((handler (tool-handler tool-record)))
          (handler args))
        (string-append "Unknown tool: " (if (string? name) name "")))))

;;; ----------------------------------------------------------------
;;; ollama-with-tools — agent loop
;;; ----------------------------------------------------------------

;; Send PROMPT to Ollama with tool calling support.
;; Runs an agent loop: if the model requests tool calls, executes
;; them and feeds results back until the model returns a final text
;; answer or MAX-ROUNDS is exhausted.
;; REGISTRY is a tool registry (from make-tool-registry / default-tool-registry).
;; Returns the final response text as a string.
(def (ollama-with-tools prompt registry
                        model: (model *tool-model*)
                        max-rounds: (max-rounds 5))
  (let* ((tool-schemas (tool-registry-schemas registry))
         (messages
           (list
             (list->hash-table
               `(("role" . "user")
                 ("content" . ,prompt))))))
    (let loop ((round 0)
               (msgs messages))
      (if (>= round max-rounds)
          (begin
            (displayln "Maximum rounds reached without a final answer.")
            "[Tool-calling loop reached maximum rounds without completing]")
          (begin
            (displayln "\n--- Round " round " ---")
            (let* ((response (ollama-chat msgs tools: tool-schemas model: model))
                   (message (hash-ref response 'message))
                   (content (hash-get message 'content))
                   (tool-calls (hash-get message 'tool_calls)))

              (cond
                ;; Model requested tool calls — execute them and loop
                ((and tool-calls (pair? tool-calls))
                 (displayln "Model requested " (length tool-calls) " tool call(s).")

                 ;; Append the assistant's message (with tool_calls) to history
                 (let ((msgs-with-assistant (append msgs (list message))))

                   ;; Execute each tool call and append results
                   (let tc-loop ((remaining tool-calls)
                                 (acc msgs-with-assistant))
                     (if (null? remaining)
                         (loop (+ round 1) acc)
                         (let* ((tc (car remaining))
                                (name (hash-ref (hash-ref tc 'function) 'name))
                                (result (dispatch-tool-call registry tc)))
                           (displayln "  [result] " name " => " result)
                           (tc-loop (cdr remaining)
                                    (append acc
                                            (list
                                             (list->hash-table
                                              `(("role" . "tool")
                                                ("tool_name" . ,name)
                                                ("content" . ,result)))))))))))

                ;; No tool calls — return the final text content
                (else
                 (displayln "Final answer received.")
                 (or content "No response content")))))))))
