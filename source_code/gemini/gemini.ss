(import :std/net/request
        :std/text/json)

(export gemini)

(def (pprint-hashtable ht)
  "Prints a hash table with line breaks and indentation."
  (hash-map (lambda (k v) (displayln "key: " k " value: " v)) ht)) 

(def (gemini
      prompt
      model: (model "gemini-3-flash-preview")
      system-prompt: (system-prompt "You are a helpful assistant."))
  (let ((api-key (get-environment-variable "GOOGLE_API_KEY")))
    (unless api-key
      (error "GOOGLE_API_KEY environment variable not set."))

    (let* ((headers `(("Content-Type". "application/json")
                         ("x-goog-api-key". ,api-key)
                         ("Api-Revision". "2026-05-20")))
           (body-data
            (list->hash-table
             `(("model". ,model)
               ("input". ,prompt))))
           (body-string (json-object->string body-data))
           (endpoint (string-append "https://generativelanguage.googleapis.com/v1beta/interactions?key=" api-key)))
      (let ((response (http-post endpoint headers: headers data: body-string)))
        ;;(displayln response)
        (if (= (request-status response) 200)
          (let* ((response-json (request-json response))
                 (steps (hash-ref response-json 'steps))
                 ;; Find the last model_output step
                 (model-step
                  (let loop ((ss (reverse steps)))
                    (cond
                      ((null? ss) #f)
                      ((equal? (hash-ref (car ss) 'type) "model_output") (car ss))
                      (else (loop (cdr ss))))))
                 (content (hash-ref model-step 'content))
                 (first-content (car content)))
            (hash-ref first-content 'text)))))))

;;  (gemini "why is the sky blue? be very concise")
