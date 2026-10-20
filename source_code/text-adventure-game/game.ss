;; File: game.ss
;; AI-driven text adventure game in Gerbil Scheme using a local Ollama model.
;; Ported from Common Lisp in "Loving Common Lisp".

(import :std/format :std/misc/ports :std/net/request :std/text/json)
(export play chat-ollama)

(def default-model "qwen3.5:4b")
(def default-story-file "story.txt")

;; Multi-turn chat with Ollama (/api/chat endpoint).
;; MESSAGES is a list of alists: ((role . "system"|"user"|"assistant") (content . "..."))
(def (chat-ollama messages model: (model default-model) api-base: (api-base "http://localhost:11434"))
  (let* ((endpoint (string-append api-base "/api/chat"))
         (json-messages
          (map (lambda (m)
                 (list->hash-table
                  `(("role" . ,(cdr (assoc 'role m)))
                    ("content" . ,(cdr (assoc 'content m))))))
               messages))
         (payload
          (list->hash-table
           `(("model" . ,model)
             ("messages" . ,json-messages)
             ("stream" . #f)
             ("think" . #f))))
         (response (http-post endpoint
                              headers: '(("Content-Type" . "application/json"))
                              data: (json-object->string payload))))
    (if (= (request-status response) 200)
      (let* ((resp-json (request-json response))
             (msg (hash-ref resp-json 'message)))
        (hash-ref msg 'content))
      (error "Ollama API request failed" (request-status response) (request-text response)))))

(def (trim-ws str)
  (let* ((n (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i n) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1))
                    i)))
         (end (let loop ((i n))
                (if (and (> i start) (char-whitespace? (string-ref str (- i 1))))
                  (loop (- i 1))
                  i))))
    (substring str start end)))

;; Interactive game loop.
(def (play story-file: (story-file default-story-file) model: (model default-model))
  (unless (file-exists? story-file)
    (error "Story file not found:" story-file))
  (let* ((story (read-file-string story-file))
         (messages `(((role . "system") (content . ,story)))))
    (displayln "\n=== Initializing Adventure ===")
    (displayln "Contacting Game Master via Ollama (" model ")...")
    ;; Initial turn: send empty user request to get opening scene
    (let* ((opening-messages (append messages '(((role . "user") (content . "Begin the game.")))))
           (opening-scene (chat-ollama opening-messages model: model)))
      (displayln "\n" opening-scene "\n")
      (set! messages (append opening-messages `(((role . "assistant") (content . ,opening-scene)))))
      (displayln "Welcome to the Text Adventure!")
      (displayln "Describe what you want to do, or type 'quit' to exit.\n")
      (let loop ()
        (display "> ")
        (flush-output-port)
        (let ((line (read-line)))
          (unless (eof-object? line)
            (let ((user-input (trim-ws line)))
              (cond
               ((member (string-downcase user-input) '("quit" "exit"))
                (displayln "Goodbye, adventurer!"))
               ((string=? user-input "")
                (loop))
               (else
                (set! messages (append messages `(((role . "user") (content . ,user-input)))))
                (let ((response (chat-ollama messages model: model)))
                  (displayln "\n" response "\n")
                  (set! messages (append messages `(((role . "assistant") (content . ,response)))))
                  (loop)))))))))))

;; Main entry point for standalone run
(def (main . args)
  (let ((model (if (pair? args) (car args) default-model)))
    (play model: model)))
