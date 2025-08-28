# Google Gemini API

The Google Gemini API provides developers with access to Google's state-of-the-art family of Gemini Large Language Models (LLMs), representing a significant leap forward in multimodal artificial intelligence. Unlike earlier models that primarily processed text, the Gemini series—comprising models like the highly capable Gemini Ultra, the versatile Gemini Pro, and the efficient Gemini Nano—was designed from the ground up to seamlessly understand, operate across, and combine different types of information, including text, code, images, audio, and video. This native multimodality allows for the development of sophisticated applications that can reason about complex inputs, such as analyzing the steps in a video, interpreting charts and diagrams within a document, or generating creative text based on a visual prompt. The API offers a streamlined and powerful interface, enabling developers to integrate these advanced reasoning and generation capabilities into their own software, pushing the boundaries of what's possible in domains ranging from data analysis and content creation to building the next generation of intelligent, context-aware user experiences.


## Example Code

In this section, we'll explore a practical example of interacting with a modern web API by building a client for Google's Gemini Large Language Model. The following program defines a function named **gemini** that takes a text prompt and returns the model's generated response. This involves several common tasks in modern software development: retrieving sensitive information like an API key from environment variables, dynamically constructing a JSON payload according to the API's specification, setting the correct HTTP headers for authentication and content type, and executing an HTTP POST request. Upon receiving a successful response, the code demonstrates how to parse the returned JSON data to extract the specific piece of information we need—the generated text from a complex, nested data structure. This example serves as a simple example for making outbound web requests and handling the data interchange that is central to working with external services.

{lang="scheme", linenos=off}
```
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

The core logic resides within the gemini function, which uses a series of let* bindings to sequentially construct the components of the API request. First, it defines the necessary HTTP headers, including the API key. Next, it builds the body-data as a nested hash table, precisely matching the structure required by the Gemini API, before serializing it into a JSON string using **json-object->string**. Finally, the full endpoint URL is assembled by concatenating the base URL with the specific model being used. The actual network communication is handled by the http-post function, which sends all the prepared information to the Google servers.

Once the http-post call returns, the program immediately checks the response status. If the request was successful (status code 200), it proceeds to parse the data; otherwise, nothing is returned. The parsing logic is a chain of data extraction operations on the JSON response, which is first converted into a Gerbil Scheme hash table. Using a combination of **hash-ref** to access values by their keys (like 'candidates and 'content) and the function **car** to access the first element of a list, the code navigates the nested data structure to isolate the desired text content. This sequence elegantly demonstrates how Gerbil Scheme's standard library functions for handling lists and hash tables can be composed to efficiently process structured data from external APIs.

## Example Output

Change directory to **source-code/gemini** an run:

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

