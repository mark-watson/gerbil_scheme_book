(import :std/net/request :std/text/json)
(export ollama)

(def (ollama prompt
             model: (model "gemma3:latest")) ;; "gpt-oss:20b")) ;; "qwen3:0.6b"))
  (let* ((endpoint "http://localhost:11434/api/generate")
         (headers '(("Content-Type". "application/json")))
         (body-data 
           (list->hash-table
             `(("model". ,model) ("prompt". ,prompt) ("stream". #f))))
         (body-string (json-object->string body-data)))

    (let ((response (http-post endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
          (let* ((response-json (request-json response)))
            ;;(displayln (hash-keys response-json))
            (hash-ref response-json 'response))
          (error "Ollama API request failed"
                 status: (request-status response)
                 body: (request-text response))))))

;;  (ollama "why is the sky blue? Be very concise.")
