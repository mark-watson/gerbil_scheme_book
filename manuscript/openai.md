# OpenAI API

The OpenAI API serves as the primary gateway for developers to harness the groundbreaking capabilities of OpenAI's suite of artificial intelligence models, most notably the influential Generative Pre-trained Transformer (GPT) series. Since the release of GPT-3, and continuing with more advanced successors like GPT-4, this API has fundamentally reshaped the landscape of software development by making sophisticated natural language understanding, generation, and reasoning accessible as a programmable service. It allows developers to integrate functionalities such as text summarization, language translation, code generation, sentiment analysis, and conversational AI into their applications through simple HTTP requests. By abstracting away the immense complexity of training and hosting these massive models, the OpenAI API has catalyzed a wave of innovation, empowering everyone from individual hobbyists to large enterprises to build intelligent applications that can write, read, and comprehend human language with unprecedented fluency and coherence.

TBD

## Example Code

```scheme
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
        (if (= (request-status response) 200)
            (let* ((response-json (request-json response))
                   (choices (hash-ref response-json 'choices))
                   (first-choice (and (pair? choices) (car choices)))
                   (message (hash-ref first-choice 'message))
                   (content (hash-ref message 'content)))
              content)
            (error "OpenAI API request failed"
                   status: (request-status response)
                   body: (request-text response)))))))

;; (openai "why is the sky blue? be very concise")
```

## Example Output

TBD

