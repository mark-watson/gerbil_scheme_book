;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; llm.ss -- OpenAI-compatible chat-completions client and the agentic
;;; tool-calling loop.
;;;
;;; Every provider in the harness config (Fireworks, DeepSeek, mlx_lm.server,
;;; oMLX, Ollama's /v1 shim, ...) speaks the same /v1/chat/completions wire
;;; format, so one non-streaming client covers them all. Endpoint, model, key
;;; env var, generation parameters and pricing all come from the active
;;; provider profile; nothing provider-specific is compiled in.

(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/13
        :std/net/request
        :std/text/json
        "config"
        "util")

(export llm-chat
        llm-chat-with-tools
        model-override
        current-model-id
        message-content
        reset-session-stats
        print-session-stats)

;; Seconds to wait for a complete (non-streaming) response. Local models
;; writing a whole Gerbil file can take minutes.
(def HTTP-TIMEOUT 900)

;; /model and --model override the active profile's model for this session.
(def model-override (make-parameter #f))

(def (current-model-id)
  (or (model-override)
      (provider-model (config-active-provider))
      "?"))

;; ---------------------------------------------------------------------------
;; Session token accounting

(def stats (hash (prompt 0) (completion 0) (total 0) (cached 0)))

(def (reset-session-stats)
  (for-each (lambda (k) (hash-put! stats k 0)) '(prompt completion total cached)))

(def (bump! key n)
  (when (number? n) (hash-put! stats key (+ (hash-ref stats key) n))))

(def (accumulate-usage data)
  (let ((usage (json-ref data 'usage)))
    (when (hash-table? usage)
      (bump! 'prompt (json-ref usage 'prompt_tokens 0))
      (bump! 'completion (json-ref usage 'completion_tokens 0))
      (bump! 'total (json-ref usage 'total_tokens 0))
      (let ((details (json-ref usage 'prompt_tokens_details)))
        (when (hash-table? details)
          (bump! 'cached (json-ref details 'cached_tokens 0)))))))

;; Rates are USD per 1M tokens from the profile's "pricing" block; a profile
;; without one reports token counts and no invented cost.
(def (print-session-stats)
  (let* ((pricing (provider-pricing (config-active-provider)))
         (in-rate (json-ref pricing 'input))
         (cached-rate (json-ref pricing 'cached_input))
         (out-rate (json-ref pricing 'output))
         (pt (hash-ref stats 'prompt))
         (ct (hash-ref stats 'completion))
         (ca (hash-ref stats 'cached)))
    (displayln "\nSession token usage:")
    (displayln (format "  Prompt tokens:     ~a" pt))
    (displayln (format "  Completion tokens: ~a" ct))
    (displayln (format "  Total tokens:      ~a" (hash-ref stats 'total)))
    (when (> ca 0)
      (displayln (format "  Cached tokens:     ~a" ca)))
    (if (or in-rate cached-rate out-rate)
      (let ((cost (+ (* (max 0 (- pt ca)) (or in-rate 0) 1e-6)
                     (* ca (or cached-rate in-rate 0) 1e-6)
                     (* ct (or out-rate 0) 1e-6))))
        (displayln (format "  Estimated cost:    $~a" (fixed-point cost 6))))
      (displayln "  Estimated cost:    n/a (no \"pricing\" block for this provider)"))))

;; :std/format's ~f prints 1.2e-4 style; dollars want 0.000120.
(def (fixed-point x digits)
  (let* ((scale (expt 10 digits))
         (n (inexact->exact (round (* x scale)))))
    (string-append (number->string (quotient n scale)) "."
                   (string-pad (number->string (remainder n scale)) digits #\0))))

;; ---------------------------------------------------------------------------
;; Low-level POST

(def (active-provider-or-error)
  (or (config-active-provider)
      (error "no active provider profile; define \"providers\" in ~/.coding_harness.json or .local_coding_harness.json")))

(def (request-headers provider)
  (let* ((key-env (provider-api-key-env provider))
         (key (and key-env (getenv key-env #f))))
    (when (and key-env (not key))
      (error (format "~a environment variable not set (needed by provider '~a')"
                     key-env (config-active-provider-name))))
    (append '(("Content-Type" . "application/json")
              ("Accept" . "application/json"))
            (if key [(cons "Authorization" (string-append "Bearer " key))] []))))

(def (post-chat payload)
  (let* ((provider (active-provider-or-error))
         (endpoint (or (provider-endpoint provider)
                       (error "active provider profile has no \"endpoint\"")))
         (body (json-object->string payload)))
    (when (debug?)
      (displayln (colorize YELLOW (format "[debug] POST ~a ~a" endpoint (truncate-string body 1500)))))
    (let* ((resp (http-post endpoint headers: (request-headers provider) data: body timeout: HTTP-TIMEOUT))
           (status (request-status resp))
           (text (request-text resp)))
      (request-close resp)
      (when (debug?)
        (displayln (colorize YELLOW (format "[debug] HTTP ~a ~a" status (truncate-string text 1500)))))
      (unless (= status 200)
        (error (format "HTTP ~a from ~a: ~a" status endpoint (truncate-string text 600))))
      (let ((data (string->json-object text)))
        (when (json-ref data 'error)
          (error (format "API error: ~a" (json-object->string (json-ref data 'error)))))
        (accumulate-usage data)
        data))))

;; Request body: model + messages + whichever generation parameters the
;; active profile declares (anything it omits uses the server default).
(def (make-payload messages (extra []))
  (let ((payload (hash (model (current-model-id)) (messages messages)))
        (gen (provider-generation (active-provider-or-error))))
    (for-each (lambda (k)
                (let ((v (json-ref gen k)))
                  (when v (hash-put! payload k v))))
              '(temperature max_tokens top_p))
    (for-each (lambda (kv) (hash-put! payload (car kv) (cdr kv))) extra)
    payload))

(def (response-message data)
  (let ((choices (json-ref data 'choices)))
    (and (pair? choices) (json-ref (car choices) 'message))))

;; Some servers send "content": null on tool-only turns.
(def (message-content msg)
  (let ((c (json-ref msg 'content "")))
    (if (string? c) c "")))

;; ---------------------------------------------------------------------------
;; Plain chat (no tools)

(def (llm-chat messages max-tokens: (max-tokens #f))
  (let* ((payload (make-payload messages))
         (_ (when max-tokens (hash-put! payload 'max_tokens max-tokens)))
         (msg (response-message (post-chat payload)))
         (content (message-content msg)))
    (if (string=? content "") "(no response content)" content)))

;; ---------------------------------------------------------------------------
;; Agentic loop
;;
;; llm-chat-with-tools : messages tool-schemas execute-fn max-iterations
;;                       -> (values final-text final-messages)
;; execute-fn maps a list of tool-call objects to a list of
;; (call-id name result-string) triples.

(def REPEAT-WINDOW 5)   ; remember the last N tool-call batches
(def REPEAT-LIMIT 2)    ; the same batch twice in the window => stuck

(def (call-signature tool-calls)
  (sort (map (lambda (tc)
               (let ((f (json-ref tc 'function)))
                 (format "~a|~a" (json-ref f 'name "") (arguments->string (json-ref f 'arguments "")))))
             tool-calls)
        string<?))

(def (arguments->string a)
  (if (string? a) a (json-object->string a)))

(def (take-at-most lst n)
  (if (or (null? lst) (= n 0)) [] (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

(def (llm-chat-with-tools messages tool-schemas execute-fn max-iterations)
  (let loop ((iter 0) (msgs messages) (recent []))
    (if (>= iter max-iterations)
      ;; Out of iterations: one final call without tools for a summary.
      (try
       (let* ((msg (response-message (post-chat (make-payload msgs))))
              (content (message-content msg)))
         (values (if (string=? content "") "(max tool iterations reached)" content)
                 (if msg (append msgs [msg]) msgs)))
       (catch (e) (values "(max tool iterations reached)" msgs)))
      (let* ((data (post-chat (make-payload msgs [(cons 'tools tool-schemas) (cons 'tool_choice "auto")])))
             (msg (or (response-message data) (error "response has no message")))
             (tool-calls (let ((tc (json-ref msg 'tool_calls))) (and (pair? tc) tc)))
             (content (message-content msg))
             (msgs (append msgs [msg])))
        (cond
         ((not tool-calls)
          (values (if (string=? (trim content) "") "(empty response from model)" content) msgs))
         ((member (call-signature tool-calls) recent)
          (values (format "(stopped: the model repeated the identical tool call(s) ~a times without progress)"
                          REPEAT-LIMIT)
                  msgs))
         (else
          (unless (string=? (trim content) "")
            (displayln (string-append "\n" (trim content))))
          (let* ((results (execute-fn tool-calls))
                 (tool-msgs (map (lambda (r)
                                   (hash (role "tool")
                                         (tool_call_id (car r))
                                         (name (cadr r))
                                         (content (caddr r))))
                                 results)))
            (loop (+ iter 1)
                  (append msgs tool-msgs)
                  (take-at-most (cons (call-signature tool-calls) recent) REPEAT-WINDOW)))))))))
