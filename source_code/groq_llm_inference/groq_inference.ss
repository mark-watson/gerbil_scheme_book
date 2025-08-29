(import :std/net/request
        :std/text/json)

(export groq_inference)

;; Generic Groq chat completion helper
;; Usage: (groq_inference model prompt [system-prompt: "..."])
(def (groq_inference
      model prompt
      system-prompt: (system-prompt "You are a helpful assistant."))
  (let ((api-key (get-environment-variable "GROQ_API_KEY")))
    (unless api-key
      (error "GROQ_API_KEY environment variable not set."))

    (let* ((headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(string-append "Bearer " api-key))))
           (body-data
            (list->hash-table
             `(("model" . ,model)
               ("messages" . ,(list
                               (list->hash-table `(("role" . "system") ("content" . ,system-prompt)))
                               (list->hash-table `(("role" . "user") ("content" . ,prompt))))))))
           (body-string (json-object->string body-data))
           (endpoint "https://api.groq.com/openai/v1/chat/completions"))
      
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
          (let* ((response-json (request-json response))
                 (choices (hash-ref response-json 'choices))
                 (first-choice (and (pair? choices) (car choices)))
                 (message (and first-choice (hash-ref first-choice 'message)))
                 (content (and message (hash-ref message 'content))))
            (or content (error "Groq response missing content")))
          (error "Groq API request failed"
            status: (request-status response)
            body: (request-text response)))))))

