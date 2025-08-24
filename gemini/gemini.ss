(import :std/net/request
        :std/text/json)

(export gemini)

(def (pprint-hashtable ht)
     "Prints a hash table with line breaks and indentation."
     (hash-map (lambda (k v) (displayln "key: " k " value: " v)) ht)) 

(def (gemini prompt
             model: (model "gemini-2.5-flash")
             system-prompt: (system-prompt "You are a helpful assistant."))
     (let ((api-key (get-environment-variable "GOOGLE_API_KEY")))
       (unless api-key
         (error "GEMINI_API_KEY environment variable not set."))

       (let* ((headers `(("Content-Type". "application/json")
                         ("x-goog-api-key". ,api-key)))
              (body-data
               (list->hash-table
                `(("contents". ,(list
                                 (list->hash-table
                                  `(("role". "user")
                                    ("parts". ,(list (list->hash-table `(("text". ,prompt))))))))))))
              (body-string (json-object->string body-data))
              (endpoint (string-append "https://generativelanguage.googleapis.com/v1beta/models/"
                                       model ":generateContent?key=" api-key)))

         (displayln endpoint)
         (displayln "body-string:\n" body-string)

         (let ((response (http-post endpoint headers: headers data: body-string)))
           (displayln "response:") (pretty-print response)
           (if (= (request-status response) 200)
               (let* ((response-json (request-json response)))
                 (displayln "response-json:") (displayln response-json)
                 (let* ((candidate (car (hash-ref response-json 'candidates)))
                        (content (hash-ref candidate 'content))
                        (p1 (car (hash-ref content 'parts))))
                   (displayln "p1: " p1)
                   ;;(displayln "content:") (pprint-hashtable content)
                   (displayln "\n------")
                   ;;(let* ((parts (hash-ref 'parts content)))
                   ;;  (displayln "parts:") (displayln parts) ;; (pprint-hashtable part)
                     (hash-ref p1 'text))))))))

;;   (gemini "why is the sky blue? be very concise")
