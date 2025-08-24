(export #t) ; export all symbols

(def (pretty-print-hash hash-table)
  (let ((alist (hash-map (lambda (k v) `(,k . ,v)) hash-table)))
    (pretty-print alist)))

;; File: openai.ss
(import :std/net/request
        :std/text/json)

(export openai)

(def (openai prompt
                 model: (model "gpt-5-mini")
                 system-prompt: (system-prompt "You are a helpful assistant."))
  (let ((api-key (get-environment-variable "OPENAI_API_KEY")))
    (unless api-key
      (error "OPENAI_API_KEY environment variable not set."))

    (let* ((headers `(("Content-Type". "application/json")
                      ("Authorization". ,(string-append "Bearer " api-key))))
           (body-data
            (list->hash-table
             `(("model". ,model)
               ("messages". ,(list
                               (list->hash-table `(("role". "system") ("content". ,system-prompt)))
                               (list->hash-table `(("role". "user") ("content". ,prompt))))))))
           (body-string (json-object->string body-data))
           (endpoint "https://api.openai.com/v1/chat/completions"))

      (let ((response (http-post endpoint headers: headers data: body-string)))
        (displayln response)
        (if (= (request-status response) 200)
            (let* ((response-json (request-json response))
                   (choices (hash-ref response-json 'choices))
                   (first-choice (and (pair? choices) (car choices)))
                   (message (hash-ref first-choice 'message))
                   (content (hash-ref message 'content)))
              (displayln "pp response-json):\n" (pretty-print-hash response-json))
              (displayln "first-coice:\n" first-choice)
              (displayln "content:\n" content)
              content)
            (error "OpenAI API request failed"
                   status: (request-status response)
                   body: (request-text response)))))))

(openai "why is the sky blue? be very concise")