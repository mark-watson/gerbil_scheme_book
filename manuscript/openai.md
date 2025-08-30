# OpenAI API

The OpenAI API serves as the primary gateway for developers to harness the groundbreaking capabilities of OpenAI's suite of artificial intelligence models, most notably the influential Generative Pre-trained Transformer (GPT) series. Since the release of GPT-3, and continuing with more advanced successors like GPT-4, this API has fundamentally reshaped the landscape of software development by making sophisticated natural language understanding, generation, and reasoning accessible as a programmable service. It allows developers to integrate functionalities such as text summarization, language translation, code generation, sentiment analysis, and conversational AI into their applications through simple HTTP requests. By abstracting away the immense complexity of training and hosting these massive models, the OpenAI API has catalyzed a wave of innovation, empowering everyone from individual hobbyists to large enterprises to build intelligent applications that can write, read, and comprehend human language with unprecedented fluency and coherence.

Here you will learn how to send prompts to the OpenAI GPT AI models. For information on creating effective prompts please read my blog article [Notes on effectively using AI](https://mark-watson.blogspot.com/2025/08/notes-on-effectively-using-ai.html).

## Example Code

This next program in file **gerbil_scheme_book/source_code/openai/openai.ss** provides another practical example of interfacing with a modern web API from Gerbil Scheme. We will define a function, **openai**, that acts as a simple client for the OpenAI Chat Completions API. This function takes a user's prompt as its primary argument and includes optional keyword arguments to specify the AI model and a system-prompt to set the context for the conversation. Before making the request, it securely retrieves the necessary API key from an environment variable, a best practice that avoids hard-coding sensitive credentials. The core logic involves constructing a proper JSON payload containing the model and messages, setting the required HTTP headers for authorization and content type, and then sending this data via an HTTP POST request. Finally, it parses the JSON response from the OpenAI servers to extract and return the generated text content from the AI model, while also including basic error handling for failed requests.

{lang="scheme", linenos=off}
```
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

The implementation begins by importing the necessary standard libraries for handling HTTP requests (:std/net/request) and JSON data manipulation (:std/text/json). Inside the **openai** function, a **let*** block is used to sequentially bind variables for the request. It first constructs the HTTP headers and the request body, which is a hash-table representing the JSON structure required by the OpenAI API, including the model name and a list of messages for the "system" and "user" roles. This hash-table is then serialized into a JSON string. The http-post procedure is called with the API endpoint, headers, and the serialized data to perform the web request.

Upon receiving a response, the code demonstrates robust handling of the result. It first checks if the HTTP status code is 200, indicating success. If the request was successful, it parses the JSON text from the response body back into a hash-table. It then carefully navigates the nested structure of this response data using a chain of hash-ref and car calls to drill down through the choices array and message object to finally extract the desired content string. If the HTTP request failed, the else branch is triggered, raising an error with the status code and the response body, which provides valuable debugging information to the user.

## Example Output

In the following example I run the Gerbil Scheme interpreter, loading the file "openai.ss", and then entering an interactive REPL:

```console
$ gxi -L openai.ss -
> (openai "why is the sky blue? be very concise")
"Because air molecules scatter shorter (blue) wavelengths of sunlight (Rayleigh scattering) more than longer wavelengths, so blue light is sent in all directions and fills the sky."
> (openai "list 3 things that the language Gerbil Scheme is most used for. Be concise.")
"- Writing high-performance native-code programs and command-line tools (runs on Gambit)\n- Rapid prototyping and DSLs using its powerful macro/metaprogramming facilities\n- Building concurrent and networked services (sockets, lightweight threads) and small web apps"
> (displayln (openai "list 3 things that the language Gerbil Scheme is most used for. Be concise."))
- Language-oriented programming and DSLs (heavy macro/metaprogramming support).  
- Server-side and networked applications/web services (runs on fast Gambit runtime).  
- Scripting, rapid prototyping and systems-level code using Gambit’s FFI and concurrency.
>
```

Notice that I repeated the second example, displaying the string response in a more readable format. As we also see in this example, Large Language Models will in general produce different output when called with the same prompt.

Sometimes we might want the output in a specific format, like JSON:

```console
$ gxi -L openai.ss -
> (displayln (openai "Be concise in your thinking and only provide one correct answer, no need to think about different correct answers for the problem: Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format."))
{"Sally-Bill":45,"Sally-Alex":33,"Bill-Alex":12}
>
```

This example is not good enough! When you use LLMs in your applications it is better to one-shot prompt with the exact output format you need in your application. Here is an example prompt:

```text
You are an information extraction system.
Extract all people’s **full names** and **email addresses** from the following text.
If no names or emails are present, return an empty list.

Return the result strictly in this JSON format:

{
  "contacts": [
    {
      "name": "<full name as written in text>",
      "email": "<email address>"
    }
  ]
}

Text:
"Hi, I’m Alice Johnson, please email me at alice.j@example.com.  
Also, you can reach Bob Smith via bob.smith42@gmail.com."
```

Let's run this one-shot prompt in a Gerbil Scheme REPL:

```console
$ gxi -L openai.ss -
> (def prompt #<<EOF
You are an information extraction system.
Extract all people’s **full names** and **email addresses** from the following text.
If no names or emails are present, return an empty list.

Return the result strictly in this JSON format:

{
  "contacts": [
    {
      "name": "<full name as written in text>",
      "email": "<email address>"
    }
  ]
}

Text:
"Hi, I’m Alice Johnson, please email me at alice.j@example.com.  
Also, you can reach Bob Smith via bob.smith42@gmail.com."
EOF
)
> (displayln (openai prompt))
{
  "contacts": [
    {
      "name": "Alice Johnson",
      "email": "alice.j@example.com"
    },
    {
      "name": "Bob Smith",
      "email": "bob.smith42@gmail.com"
    }
  ]
}
> 
```
