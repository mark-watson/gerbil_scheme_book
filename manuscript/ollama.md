# Ollama

Ollama is a powerful and user-friendly tool designed to simplify the process of running large language models (LLMs) locally on personal hardware. In a landscape often dominated by cloud-based APIs, Ollama democratizes access to advanced AI by providing a simple command-line interface that bundles model weights, configurations, and a tailored execution environment into a single, easy-to-install package. It allows developers, researchers, and enthusiasts to download and interact with a wide range of popular open-source models, such as Llama 3, Mistral, and Phi-3, with just a single command. Beyond its interactive chat functionality, Ollama also exposes a local REST API, enabling the seamless integration of these locally-run models into custom applications without the latency, cost, or privacy concerns associated with remote services. This focus on accessibility and local deployment makes it an indispensable tool for offline development, rapid prototyping, and leveraging the power of modern LLMs while maintaining full control over data and infrastructure.

TBD

## Example Code

```scheme
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
```

## Example Output

TBD

