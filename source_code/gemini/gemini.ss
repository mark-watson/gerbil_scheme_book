(import :std/net/request
        :std/text/json)

(export gemini)

(def (gemini
      prompt
      model: (model "gemini-3-flash-preview")
      system-prompt: (system-prompt #f))
  (let ((api-key (getenv "GOOGLE_API_KEY")))
    (unless api-key
      (error "GOOGLE_API_KEY environment variable not set."))

    (let* ((headers `(("Content-Type" . "application/json")
                      ("x-goog-api-key" . ,api-key)
                      ("Api-Revision" . "2026-05-20")))
           (body-fields `(("model" . ,model)
                          ("input" . ,prompt)))
           (body-fields (if system-prompt
                            (cons `("system_instruction" .
                                    ,(list->hash-table
                                      `(("parts" . (,(list->hash-table `(("text" . ,system-prompt))))))))
                                  body-fields)
                            body-fields))
           (body-data (list->hash-table body-fields))
           (body-string (json-object->string body-data))
           (endpoint "https://generativelanguage.googleapis.com/v1beta/interactions"))
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
          (let* ((response-json (request-json response))
                 (steps (hash-ref response-json 'steps #f))
                 (model-step
                  (and (pair? steps)
                       (let loop ((ss (reverse steps)))
                         (cond
                          ((null? ss) #f)
                          ((equal? (hash-ref (car ss) 'type #f) "model_output") (car ss))
                          (else (loop (cdr ss)))))))
                 (content (and model-step (hash-ref model-step 'content #f)))
                 (first-content (and (pair? content) (car content))))
            (if first-content
                (hash-ref first-content 'text "")
                (error "Gemini API: no model_output found in response")))
          (error "Gemini API request failed"
                 status: (request-status response)
                 body: (request-text response)))))))

;;  (gemini "why is the sky blue? be very concise")
