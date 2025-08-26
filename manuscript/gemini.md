# Google Gemini API

The Google Gemini API provides developers with access to Google's state-of-the-art family of large language models, representing a significant leap forward in multimodal artificial intelligence. Unlike earlier models that primarily processed text, the Gemini series—comprising models like the highly capable Gemini Ultra, the versatile Gemini Pro, and the efficient Gemini Nano—was designed from the ground up to seamlessly understand, operate across, and combine different types of information, including text, code, images, audio, and video. This native multimodality allows for the development of sophisticated applications that can reason about complex inputs, such as analyzing the steps in a video, interpreting charts and diagrams within a document, or generating creative text based on a visual prompt. The API offers a streamlined and powerful interface, enabling developers to integrate these advanced reasoning and generation capabilities into their own software, pushing the boundaries of what's possible in domains ranging from data analysis and content creation to building the next generation of intelligent, context-aware user experiences.

TBD

## Example Code

TBD

```scheme
(import :std/net/request
        :std/text/json)

(export gemini)

(def (pprint-hashtable ht)
  "Prints a hash table with line breaks and indentation."
  (hash-map (lambda (k v) (displayln "key: " k " value: " v)) ht)) 

(def (gemini
      prompt
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
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (displayln response)
        (if (= (request-status response) 200)
          (let* ((response-json (request-json response))
                 (candidate (car (hash-ref response-json 'candidates)))
                 (content (hash-ref candidate 'content))
                 (p1 (car (hash-ref content 'parts))))
            (hash-ref p1 'text)))))))

;;  (gemini "why is the sky blue? be very concise")
```

## Example Output

TBD

