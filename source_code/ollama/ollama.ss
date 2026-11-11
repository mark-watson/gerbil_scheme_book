(import :std/net/request :std/text/json :std/srfi/13)
(export ollama)

;; Determine base endpoint from OLLAMA_HOST or default to localhost:11434
(def (ollama-host)
  (let ((host (or (getenv "OLLAMA_HOST" #f) "http://localhost:11434")))
    (if (string-prefix? "http" host)
        host
        (string-append "http://" host))))

(def (ollama prompt
             model: (model "gemma3:latest"))
  (let* ((endpoint (string-append (ollama-host) "/api/generate"))
         (headers '(("Content-Type" . "application/json")))
         (body-data 
           (list->hash-table
             `(("model" . ,model) ("prompt" . ,prompt) ("stream" . #f))))
         (body-string (json-object->string body-data)))
    (with-catch
      (lambda (e)
        (error "Could not connect to Ollama. Is the Ollama server running?" irritant: e))
      (lambda ()
        (let ((response (http-post endpoint headers: headers data: body-string)))
          (if (= (request-status response) 200)
              (let* ((response-json (request-json response)))
                (hash-ref response-json 'response))
              (error "Ollama API request failed"
                     status: (request-status response)
                     body: (request-text response))))))))
