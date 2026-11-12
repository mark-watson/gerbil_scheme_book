# Google Gemini API

The Google Gemini API provides developers with access to Google's family of Gemini Large Language Models (LLMs), representing a significant leap forward in multimodal artificial intelligence. These API offers a streamlined and powerful interface, enabling developers to integrate these advanced reasoning and generation capabilities into their own software, pushing the boundaries of what's possible in domains ranging from data analysis and content creation to building the next generation of intelligent, context-aware user experiences.


## Example Code

In this section, we'll explore a practical example of interacting with a modern web API by building a client for Google's Gemini Large Language Model. The following program defines a function named **gemini** that takes a text prompt and returns the model's generated response. This involves several common tasks in modern software development: retrieving sensitive information like an API key from environment variables, dynamically constructing a JSON payload according to the API's specification, setting the correct HTTP headers for authentication and content type, and executing an HTTP POST request. Upon receiving a successful response, the code demonstrates how to parse the returned JSON data to extract the specific piece of information we need: the generated text from a complex, nested data structure. This serves as a simple example for making outbound web requests and handling the data interchange that is central to working with external services.

```scheme
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
```

The core logic resides within the `gemini` function, which uses `let*` bindings to construct the request. It defines the required HTTP headers, including `x-goog-api-key` and the `Api-Revision` header. Next, it builds the body data as a hash table with `model`, `input`, and optional `system_instruction` fields, serializing it into a JSON string using `json-object->string`. The endpoint points to Google's Interactions API endpoint (`/v1beta/interactions`). The network communication is handled by `http-post`.

Once `http-post` returns, the program checks the response status. If the request was successful (HTTP 200), it parses the JSON response and walks the `steps` array to find the `model_output` step, extracting the text from the first content part. If the request fails or the model returns an error, a descriptive exception is raised with the HTTP status and body text. This demonstrates how Gerbil Scheme's standard library functions for lists and hash tables compose cleanly to process structured API data.

The following architecture diagram shows the overall structure of this example, illustrating how the Gerbil Scheme client constructs an HTTP request, communicates with the Google Gemini API endpoint, and parses the JSON response to extract the generated text.

{width: "80%"}
![Architecture diagram for the Gemini API example](gemini_architecture.png)

## Example Output

Change directory to **source_code/gemini** and run:

```console
$ gxi -L gemini.ss -
> (gemini "why is the sky blue? be very concise")
"Earth's atmosphere scatters blue light more than other colors."

> (gemini "Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format. Be concise and only provide a correct answer, no need to think about different correct answers.")
"```json\n{\n  \"Sally_Bill\": 45,\n  \"Sally_Alex\": 33,\n  \"Bill_Alex\": 12\n}\n```"

> (gemini "Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format. Be concise and only provide a correct answer, no need to think about different correct answers. Only return the JSON text, don't add markdown like ```json")
"{\"Sally_Bill\": 45, \"Sally_Alex\": 33, \"Bill_Alex\": 12}"

> (displayln (gemini "Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format. Be concise and only provide a correct answer, no need to think about different correct answers. Only return the JSON text, don't add markdown like ```json"))
{"Sally_Bill": 45, "Sally_Alex": 33, "Bill_Alex": 12}
```

Notice how Gemini initially returned the JSON results in Markdown format and I modified the prompt to get the output format I wanted. Another good technique is to give LLMs an example of the output format you want in the prompt.

## Optional Practice Problems

1. **Streaming Responses**: Modify `gemini.ss` to parse and display streaming responses from the Gemini API instead of waiting for the full response to complete.
2. **System Instruction Payload**: The parameter `system-prompt` in `gemini.ss` is defined but not actually sent to the API. Update the request body structure to properly pass the system instructions according to Google Gemini's API payload specification.
3. **Robust Error Handling**: Implement exponential backoff retry logic to handle rate limit (HTTP 429) errors gracefully.


