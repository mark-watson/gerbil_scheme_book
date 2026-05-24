;;; -*- Gerbil -*-
;;; daily_use/main.ss — Interactive Gemini REPL with search grounding and cache
;;;
;;; Commands:
;;;   <text>          Ask Gemini a question (plain, no search)
;;;   !<text>         Ask Gemini with Google Search grounding
;;;   >               Add last answer to the persistent cache
;;;   !               Clear cache entries older than one week
;;;   h / H / help    Show help
;;;   q / quit / exit Exit the REPL
;;;   Ctrl-D          Exit the REPL

(import :std/net/request
        :std/text/json
        :std/sugar
        ./cache)

(export main)

(def *model* "gemini-3-flash-preview")

(def *cache* #f)
(def *last-answer* #f)

(def *stop-words*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "do" "does" "did" "will" "would" "shall" "should"
    "may" "might" "must" "can" "could" "am" "it" "its"
    "in" "on" "at" "to" "for" "of" "with" "by" "from" "as"
    "and" "or" "but" "not" "no" "nor" "so" "yet"
    "this" "that" "these" "those" "what" "which" "who" "whom"
    "i" "me" "my" "we" "our" "you" "your" "he" "she" "they" "them"
    "how" "when" "where" "why" "if" "then" "than" "about"))

(def (extract-keywords text)
  (let* ((downcased (string-downcase text))
         (words (string-split downcased))
         (cleaned (map (lambda (w)
                         (string-trim-punctuation w))
                       words)))
    (filter (lambda (w)
              (and (> (string-length w) 2)
                   (not (member w *stop-words*))))
            cleaned)))

(def (build-context-from-cache query)
  (let* ((keywords (extract-keywords query))
         (items (if (pair? keywords)
                   (cache-lookup *cache* keywords 10)
                   '())))
    (if (pair? items)
      (let ((ctx "Use the following context from previous conversations when answering:\n\n"))
        (for-each (lambda (item)
                    (set! ctx (string-append ctx "- " item "\n")))
                  items)
        (string-append ctx "---\n\n"))
      "")))

(def (%gemini-post body-data)
  (let ((api-key (getenv "GOOGLE_API_KEY")))
    (unless api-key
      (error "GOOGLE_API_KEY environment variable not set."))
    (let* ((headers `(("Content-Type" . "application/json")
                      ("x-goog-api-key" . ,api-key)
                      ("Api-Revision" . "2026-05-20")))
           (body-string (json-object->string body-data))
           (endpoint (string-append
                      "https://generativelanguage.googleapis.com/v1beta/interactions?key="
                      api-key))
           (response (http-post endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
        (let* ((response-json (request-json response))
               (steps (hash-ref response-json 'steps))
               (model-step
                 (let lp ((ss (reverse steps)))
                   (cond
                     ((null? ss) #f)
                     ((equal? (hash-ref (car ss) 'type) "model_output") (car ss))
                     (else (lp (cdr ss))))))
               (content (hash-ref model-step 'content))
               (first-content (car content)))
          (hash-ref first-content 'text))
        (error "Gemini API request failed, status: "
               (request-status response))))))

(def (ask-gemini prompt search-p: (search-p #f))
  (let ((context (build-context-from-cache prompt)))
    (let ((full-prompt (string-append context prompt)))
      (with-catch
        (lambda (e)
          (string-append "[Error calling Gemini API: " e "]"))
        (lambda ()
          (if search-p
            (let* ((search-tool (list->hash-table '(("type" . "google_search"))))
                   (body-data
                     (list->hash-table
                       `(("model" . ,*model*)
                         ("input" . ,full-prompt)
                         ("tools" . ,(list search-tool))))))
              (%gemini-post body-data))
            (let ((body-data
                    (list->hash-table
                      `(("model" . ,*model*)
                        ("input" . ,full-prompt)))))
              (%gemini-post body-data))))))))

(def (print-help)
  (displayln "")
  (displayln "  Gemini Daily-Use REPL")
  (displayln "  ─────────────────────────────────────────")
  (displayln "  <text>         Ask Gemini a question")
  (displayln "  !<text>        Ask with Google Search grounding")
  (displayln "  >              Add last answer to cache")
  (displayln "  !              Clear cache entries older than 1 week")
  (displayln "  h / help       Show this help")
  (displayln "  q / quit       Exit")
  (displayln "  Ctrl-D         Exit")
  (displayln "  ─────────────────────────────────────────")
  (displayln "  Model: " *model*)
  (displayln "  Cache: ~/.daily-use-cache-gerbil.db (" (cache-count *cache*) " items)")
  (displayln ""))

(def (display-answer text)
  (if text
    (begin
      (displayln "")
      (displayln text)
      (displayln "")
      (set! *last-answer* text))
    (displayln "\n  [No response from Gemini — check model name or API key]\n")))

(def (repl-loop)
  (displayln "\n  Gemini Daily-Use REPL  (type 'h' for help)\n")
  (let lp ()
    (display "gemini> ")
    (let ((input (read-line)))
      (if (eof-object? input)
        (displayln "\nGoodbye.")
        (let ((trimmed (string-trim input)))
          (cond
            ((string=? trimmed "")
             (lp))

            ((member trimmed '("q" "quit" "exit"))
             (displayln "Goodbye."))

            ((member trimmed '("h" "help"))
             (print-help)
             (lp))

            ((string=? trimmed ">")
             (if *last-answer*
               (begin
                 (cache-add *cache* *last-answer*)
                 (displayln "  [Cached. " (cache-count *cache*) " items total]"))
               (displayln "  [No answer to cache yet]"))
             (lp))

            ((string=? trimmed "!")
             (let ((before (cache-count *cache*)))
               (cache-clear-older-one-week *cache*)
               (let ((after (cache-count *cache*)))
                 (displayln "  [Cleared " (- before after)
                            " old entries. " after " items remain]")))
             (lp))

            ((string-prefix? "!" trimmed)
             (let ((query (string-trim (substring trimmed 1
                                                  (string-length trimmed)))))
               (if (string=? query "")
                 (let ((before (cache-count *cache*)))
                   (cache-clear-older-one-week *cache*)
                   (let ((after (cache-count *cache*)))
                     (displayln "  [Cleared " (- before after)
                                " old entries. " after " items remain]")))
                 (begin
                   (displayln "  [Searching...]")
                   (force-output)
                   (display-answer (ask-gemini query search-p: #t)))))
             (lp))

            (else
              (displayln "  [Thinking...]")
              (force-output)
              (display-answer (ask-gemini trimmed))
              (lp))))))))

(def (main . args)
  (let ((cache-path (string-append (getenv "HOME" "")
                                   "/.daily-use-cache-gerbil.db")))
    (set! *cache* (make-cache cache-path))
    (set! *last-answer* #f)
    (repl-loop)
    (cache-close *cache*)
    (displayln "  [Cache closed]")))

(def (string-trim str)
  (let ((len (string-length str)))
    (let ((start (let lp ((i 0))
                   (if (>= i len) len
                     (if (or (char=? (string-ref str i) #\space)
                             (char=? (string-ref str i) #\tab))
                       (lp (+ i 1))
                       i))))
          (end (let lp ((i (- len 1)))
                 (if (< i 0) 0
                   (if (or (char=? (string-ref str i) #\space)
                           (char=? (string-ref str i) #\tab))
                     (lp (- i 1))
                     (+ i 1))))))
      (if (>= start end) ""
        (substring str start end)))))

(def (string-trim-punctuation str)
  (let ((punct '(#\? #\! #\. #\, #\; #\: #\" #\' #\( #\)))
        (len (string-length str)))
    (let ((start (let lp ((i 0))
                   (if (>= i len) len
                     (if (memv (string-ref str i) punct)
                       (lp (+ i 1))
                       i))))
          (end (let lp ((i (- len 1)))
                 (if (< i 0) 0
                   (if (memv (string-ref str i) punct)
                     (lp (- i 1))
                     (+ i 1))))))
      (if (>= start end) ""
        (substring str start end)))))

(def (string-split str)
  (let* ((chars (string->list str))
         (words '())
         (current ""))
    (for-each (lambda (ch)
                (if (or (char=? ch #\space)
                        (char=? ch #\tab))
                  (begin
                    (when (> (string-length current) 0)
                      (set! words (cons current words))
                      (set! current "")))
                  (set! current (string-append current (string ch)))))
              chars)
    (when (> (string-length current) 0)
      (set! words (cons current words)))
    (reverse words)))
