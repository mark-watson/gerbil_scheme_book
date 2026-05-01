# Ollama

Ollama is a powerful and user-friendly tool designed to simplify the process of running large language models (LLMs) locally on personal hardware. In a landscape often dominated by cloud-based APIs, Ollama democratizes access to advanced AI by providing a simple command-line interface that bundles model weights, configurations, and a tailored execution environment into a single, easy-to-install package. It allows developers, researchers, and enthusiasts to download and interact with a wide range of popular open-source models, such as Llama 3, Mistral, and Phi-3, with just a single command. Beyond its interactive chat functionality, Ollama also exposes a local REST API, enabling the seamless integration of these locally-run models into custom applications without the latency, cost, or privacy concerns associated with remote services. This focus on accessibility and local deployment makes it an indispensable tool for offline development, rapid prototyping, and leveraging the power of modern LLMs while maintaining full control over data and infrastructure.


## Example Code


This next program in file **gerbil_scheme_book/source_code/ollama/ollama.ss** provides a practical demonstration of network programming and data handling in Gerbil Scheme by creating a simple client for the Ollama API. Ollama is a fantastic tool that allows you to run powerful large language models, like Llama 3, Mistral, and Gemma, directly on your own machine. Our **ollama** function will encapsulate the entire process of communicating with a locally running Ollama instance. It will take a text prompt as input, construct the necessary JSON payload specifying the model and prompt, send it to the Ollama server's /api/generate endpoint via an HTTP POST request, and then carefully parse the server's JSON response. The goal is to extract and return only the generated text, while also including basic error handling to gracefully manage any non-successful API responses, making for a robust and reusable utility.

{lang="scheme", linenos=off}
```
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

The **ollama** function begins by using a **let*** block to define the necessary components for the API request: the server endpoint, the required HTTP headers, and the request body-data. The body is first constructed as a Gerbil hash-table, which is the natural way to represent a JSON object, and then serialized into a JSON string using **json-object->string**. Note that the "stream" parameter is explicitly set to #f to ensure we receive the complete response at once rather than as a series of events. The core of the function is the **http-post** call, which performs the actual network request.

After the request is made, the code immediately checks the status of the response. A status code of 200 indicates success, prompting the code to parse the JSON body using **request-json** and extract the generated text from the 'response field of the resulting hash-table. If the request fails for any reason, a descriptive error is raised, including the HTTP status and response body, which is crucial for debugging. The function's design, with its optional **model:** keyword argument, makes it trivial to switch between different models you have downloaded through Ollama, providing a flexible interface for interacting with local large language models.

## Install Ollama and Pull a Model to Experiment With

**Linux Installation**

Open your terminal and run the following command to download and execute the installation script:

```bash
curl -fsSL https://ollama.com/install.sh | sh
```

**macOS Installation**

- Download the Ollama application from the official website: [https://ollama.com/download}(https://ollama.com/download).
- Unzip the downloaded file.
- Move the Ollama.app file to your /Applications folder.
- Run the application. An Ollama icon will appear in the menu bar.

This will also install the **ollama** command line program.

**Pulling the Model**

After installing Ollama on either Linux or macOS, open your terminal and run the following command to download the gemma3:latest model:

```bash
ollama pull gemma3:latest
```

After this is complete, you can run the local  API service using:

```bash
$ ollama serve
time=2025-08-26T16:05:50.161-07:00 level=INFO source=routes.go:1318 msg="server config" env="map[HTTPS_PROXY: HTTP_PROXY: NO_PROXY: OLLAMA_CONTEXT_LENGTH:4096 OLLAMA_DEBUG:INFO OLLAMA_FLASH_ATTENTION:false OLLAMA_GPU_OVERHEAD:0 OLLAMA_HOST:http://127.0.0.1:11434 OLLAMA_KEEP_ALIVE:5m0s OLLAMA_KV_CACHE_TYPE: OLLAMA_LLM_LIBRARY: OLLAMA_LOAD_TIMEOUT:5m0s OLLAMA_MAX_LOADED_MODELS:0 OLLAMA_MAX_QUEUE:512 OLLAMA_MODELS:/Users/markw/.ollama/models OLLAMA_MULTIUSER_CACHE:false OLLAMA_NEW_ENGINE:false OLLAMA_NEW_ESTIMATES:false OLLAMA_NOHISTORY:false OLLAMA_NOPRUNE:false OLLAMA_NUM_PARALLEL:1 OLLAMA_ORIGINS:[http://localhost https://localhost http://localhost:* https://localhost:* http://127.0.0.1
```

## Example Output

You need to have Ollama installed on your system and you should pull the model you want to experiment with. 

```bash
$ gxi -L ollama.ss -
> (ollama "why is the sky blue? Be very concise.")
"The sky is blue due to a phenomenon called **Rayleigh scattering**. Shorter wavelengths of light (like blue) are scattered more by the Earth's atmosphere, making the sky appear blue to our eyes."

> (ollama "write a bash script to rename all files with extension **.JPG** to **.jpg**. Just output the bash script and nothing else.")
"```bash\n#!/bin/bash\n\nfind . -name \"*.JPG\" -print0 | while IFS= read -r -d $'\\0' file; do\n  new_name=$(echo \"$file\" | sed 's/\\.JPG/.jpg/')\n  mv \"$file\" \"$new_name\"\ndone\n```\n"

> (displayln (ollama "write a bash script to rename all files with extension **.JPG** to **.jpg**. Just output the bash script and nothing else."))
``bash
#!/bin/bash

find . -name "*.JPG" -print0 | while IFS= read -r -d $'\0' file; do
  new_name=$(echo "$file" | sed 's/\.JPG/\.jpg/')
  mv "$file" "$new_name"
done
``
>
```

A few comments: In the second example I added "Just output the bash script and nothing else." to the end of the prompt. Without this, the model will generate a 100 lines of design notes, instructions how to make the bash script executable, etc. I didn't want that, just the bash script.

In the third example, I used the same prompt but used **displayln** to print the result in a more useful format.

The following architecture diagram provides an overview of how the Ollama client interacts with the locally running Ollama server, showing the flow from prompt construction through the HTTP POST request to the `/api/generate` endpoint and the JSON response parsing.

{width: "80%"}
![Architecture diagram for the Ollama example](ollama_architecture.png)

## Tool/Function Calling with Ollama

Modern LLMs can do more than generate text — they can also decide to call functions. Tool calling (sometimes called function calling) allows a model to inspect a set of tool descriptions you provide, and when appropriate, respond with a structured request to invoke one of those tools rather than producing a plain text answer. Your application then executes the tool, feeds the result back to the model, and the model incorporates that real-world data into its final answer. This creates a powerful agent loop: the model reasons about what information it needs, your code fetches it, and the model synthesizes a response.

Ollama supports tool calling through its **/api/chat** endpoint (the **/api/generate** endpoint used in the previous section does not support tools). We will split the implementation across two files: **example_tools.ss** defines the tool registry and example handlers, while **use_tools.ss** implements the chat client and agent loop.

## Defining Tools: example_tools.ss

The file **gerbil_scheme_book/source_code/ollama/example_tools.ss** establishes a small framework for defining tools that an LLM can call. Each tool has four properties: a name (the identifier the model uses), a description (so the model understands when to use it), a JSON-schema-style parameter specification, and a handler — a Gerbil procedure that actually performs the work and returns a string result.

{lang="scheme", linenos=off}
```
(import :std/text/json)
(export make-tool-registry
        register-tool!
        tool-registry-lookup
        tool-registry-schemas
        get-weather-handler
        calculate-handler
        default-tool-registry)

;;; Tool registry — a hash table mapping tool name -> tool record

(def (make-tool-registry)
  "Create an empty tool registry (hash table)."
  (make-hash-table))

(def (register-tool! registry name description parameters handler)
  "Register a tool in the registry.
   NAME: string — the function name the LLM will use.
   DESCRIPTION: string — human-readable description.
   PARAMETERS: hash table — JSON-schema-style parameter spec.
   HANDLER: procedure — (lambda (args-hash) ...) -> string."
  (hash-put! registry name
    (list->hash-table
      `(("name" . ,name)
        ("description" . ,description)
        ("parameters" . ,parameters)
        ("handler" . ,handler)))))

(def (tool-registry-lookup registry name)
  "Look up a tool record by name.  Returns #f if not found."
  (hash-get registry name))

(def (tool-registry-schemas registry . tool-names)
  "Return a list of tool schemas (suitable for the Ollama API 'tools' field).
   If TOOL-NAMES is empty, return schemas for all registered tools.
   Each schema is a hash table with 'type' and 'function' keys."
  (let ((names (if (null? tool-names)
                   (hash-keys registry)
                   tool-names)))
    (map (lambda (n)
           (let ((tool (tool-registry-lookup registry n)))
             (when tool
               (list->hash-table
                 `(("type" . "function")
                   ("function" .
                    ,(list->hash-table
                       `(("name" . ,(hash-ref tool "name"))
                         ("description" . ,(hash-ref tool "description"))
                         ("parameters" . ,(hash-ref tool "parameters"))))))))))
         names)))
```

The registry is simply a hash table keyed by tool name. The function **register-tool!** stores each tool's metadata and handler together, and **tool-registry-schemas** produces the list of JSON-compatible schema objects that will be sent to the Ollama API in the **tools** field of each request.

Next we define two example tool handlers and register them into a **default-tool-registry** that is ready to use:

{lang="scheme", linenos=off}
```
;;; Example tool handlers

(def (get-weather-handler args)
  "Handler for the get_weather tool.
   ARGS is a hash table; expects key 'location'."
  (let ((location (or (hash-get args 'location) "Unknown")))
    (string-append "Weather in " (if (string? location) location "Unknown")
                   ": Sunny, 72°F")))

(def (calculate-handler args)
  "Handler for the calculate tool.
   ARGS is a hash table; expects key 'expression'."
  (let ((expression (hash-get args 'expression)))
    (if expression
        (with-catch
          (lambda (e) (string-append "Error calculating: "
                        (with-output-to-string
                          (lambda () (display-exception e)))))
          (lambda ()
            (let ((result (eval (read (open-input-string expression)))))
              (string-append "Result: " (number->string result)))))
        "No expression provided")))

;;; Pre-built default registry with get_weather and calculate

(def default-tool-registry
  (let ((reg (make-tool-registry)))

    ;; get_weather
    (register-tool! reg
      "get_weather"
      "Get current weather for a location"
      (list->hash-table
        `(("type" . "object")
          ("properties" .
           ,(list->hash-table
              `(("location" .
                 ,(list->hash-table
                    `(("type" . "string")
                      ("description" . "The city name")))))))
          ("required" . ("location"))))
      get-weather-handler)

    ;; calculate
    (register-tool! reg
      "calculate"
      "Perform a mathematical calculation"
      (list->hash-table
        `(("type" . "object")
          ("properties" .
           ,(list->hash-table
              `(("expression" .
                 ,(list->hash-table
                    `(("type" . "string")
                      ("description" . "Math expression like (+ 2 2)")))))))
          ("required" . ("expression"))))
      calculate-handler)

    reg))
```

The **get-weather-handler** is intentionally simple — it always returns "Sunny, 72°F" — since its purpose is to demonstrate the tool-calling protocol rather than integrate with a real weather API. The **calculate-handler** is more interesting: it reads a Scheme expression from the string the model provides, evaluates it with **eval**, and returns the result. The **with-catch** form ensures that malformed expressions produce a friendly error message rather than crashing the program.

Each tool is registered with a JSON-schema describing its parameters. This schema is what the model reads to understand what arguments the tool expects. When you build your own tools, you will follow the same pattern: write a handler function, describe its parameters as a JSON-schema hash table, and call **register-tool!**.

## The Agent Loop: use_tools.ss

The file **gerbil_scheme_book/source_code/ollama/use_tools.ss** builds on the tool registry to implement a complete tool-calling client. It uses Ollama's **/api/chat** endpoint, which supports the OpenAI-compatible chat completions format including a **tools** field. The key difference from our simple **ollama** function is that instead of sending a single prompt and returning the response, we run a loop: send the prompt with tool schemas, check whether the model wants to call a tool, execute the tool if so, feed the result back, and repeat until the model returns a plain text answer.

{lang="scheme", linenos=off}
```
(import :std/net/request
        :std/text/json
        "example_tools")

(export ollama-with-tools
        ollama-chat)

(def *tool-model* "qwen3:1.7b")   ;; good small model for tool calling

(def (ollama-chat messages
                  tools: (tools #f)
                  model: (model *tool-model*))
  "Send a chat request to Ollama's /api/chat endpoint.
   MESSAGES: list of hash tables, each with 'role' and 'content' keys.
   TOOLS: optional list of tool schemas.
   MODEL: model name string.
   Returns the parsed JSON response as a hash table."
  (let* ((endpoint "http://localhost:11434/api/chat")
         (headers '(("Content-Type" . "application/json")))
         (body-data
           (list->hash-table
             (append
               `(("model" . ,model)
                 ("stream" . #f)
                 ("messages" . ,messages))
               (if tools `(("tools" . ,tools)) '()))))
         (body-string (json-object->string body-data)))
    (let ((response (http-post endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
          (request-json response)
          (error "Ollama chat API request failed"
                 status: (request-status response)
                 body: (request-text response))))))
```

The **ollama-chat** function is a lower-level building block: it takes a list of message hash tables (each with "role" and "content" keys), an optional list of tool schemas, and a model name. It constructs the JSON payload, sends it to **/api/chat**, and returns the parsed response. Notice how the **tools** field is only included when tool schemas are provided — this means **ollama-chat** can also be used for plain chat completions without tools.

{lang="scheme", linenos=off}
```
(def (dispatch-tool-call registry tool-call)
  "Execute a single tool call using handlers from REGISTRY.
   TOOL-CALL is a hash table with 'function' containing 'name'
   and 'arguments'. Returns the result string."
  (let* ((func-info (hash-ref tool-call 'function))
         (name (hash-ref func-info 'name))
         (args (hash-ref func-info 'arguments))
         (tool-record (tool-registry-lookup registry name)))
    (displayln "  [tool-call] " name " args: " args)
    (if tool-record
        (let ((handler (hash-ref tool-record "handler")))
          (handler args))
        (string-append "Unknown tool: " (if (string? name) name "")))))
```

The **dispatch-tool-call** function bridges the model's structured tool-call response and our registered handlers. When Ollama returns a tool call, the response contains a hash table with a "function" key holding the function name and an "arguments" hash table. We look up the name in our registry, retrieve the handler, and call it with the arguments.

{lang="scheme", linenos=off}
```
(def (ollama-with-tools prompt registry
                        model: (model *tool-model*)
                        max-rounds: (max-rounds 5))
  "Send PROMPT to Ollama with tool calling support.
   Runs an agent loop: if the model requests tool calls, executes
   them and feeds results back until the model returns a final text
   answer or MAX-ROUNDS is exhausted.

   REGISTRY is a tool registry (from make-tool-registry / default-tool-registry).
   Returns the final response text as a string."
  (let* ((tool-schemas (tool-registry-schemas registry))
         (messages
           (list
             (list->hash-table
               `(("role" . "user")
                 ("content" . ,prompt))))))
    (let loop ((round 0)
               (msgs messages))
      (when (>= round max-rounds)
        (error "Tool-calling loop exceeded max rounds" max-rounds))

      (displayln "\n--- Round " round " ---")
      (let* ((response (ollama-chat msgs tools: tool-schemas model: model))
             (message (hash-ref response 'message))
             (content (hash-get message 'content))
             (tool-calls (hash-get message 'tool_calls)))

        (cond
          ;; Model requested tool calls — execute them and loop
          ((and tool-calls (pair? tool-calls))
           (displayln "Model requested " (length tool-calls) " tool call(s).")

           ;; Append the assistant's message (with tool_calls) to history
           (let ((msgs-with-assistant (append msgs (list message))))

             ;; Execute each tool call and append results
             (let tc-loop ((remaining tool-calls)
                           (acc msgs-with-assistant))
               (if (null? remaining)
                   (loop (+ round 1) acc)
                   (let* ((tc (car remaining))
                          (result (dispatch-tool-call registry tc))
                          (name (hash-ref (hash-ref tc 'function) 'name)))
                     (displayln "  [result] " name " => " result)
                     (tc-loop (cdr remaining)
                              (append acc
                                (list
                                  (list->hash-table
                                    `(("role" . "tool")
                                      ("content" . ,result)))))))))))

          ;; No tool calls — return the final text content
          (else
           (displayln "Final answer received.")
           (or content "No response content")))))))
```

The **ollama-with-tools** function is the main entry point. It accepts a prompt string, a tool registry, an optional model name, and a **max-rounds** safety limit. The function uses a named **let loop** to implement the agent cycle:

1. It sends the current message history (with tool schemas) to **ollama-chat**.
2. It inspects the model's response for **tool_calls**. If present, it executes each tool call via **dispatch-tool-call**, appends both the assistant's message and the tool results to the conversation history, and loops back to step 1.
3. If no tool calls are requested, the model has produced its final answer, which is returned as a string.

The inner **tc-loop** handles the case where the model requests multiple tool calls in a single response, processing each one sequentially and accumulating the results into the message history.

## Tool Calling Example Output

To test tool calling, you need to pull a model that supports it. The **qwen3:1.7b** model works well:

```bash
ollama pull qwen3:1.7b
```

Then start a REPL session:

```console
$ gxi
> (import "example_tools" "use_tools")
> (ollama-with-tools "What's the weather like in Paris?" default-tool-registry)

--- Round 0 ---
Model requested 1 tool call(s).
  [tool-call] get_weather args: #<table>
  [result] get_weather => Weather in Paris: Sunny, 72°F

--- Round 1 ---
Final answer received.
"The weather in Paris is currently sunny with a temperature of 72°F."
```

In this example the model recognized that it needed weather data, emitted a structured tool call with `{"name": "get_weather", "arguments": {"location": "Paris"}}`, our handler returned the canned weather string, and in the second round the model composed a natural language answer incorporating the tool result.

You can also test the **calculate** tool:

```console
> (ollama-with-tools "What is 42 * 17?" default-tool-registry)

--- Round 0 ---
Model requested 1 tool call(s).
  [tool-call] calculate args: #<table>
  [result] calculate => Result: 714

--- Round 1 ---
Final answer received.
"42 multiplied by 17 equals 714."
```

## Building Your Own Tools

To add a new tool, follow three steps:

1. **Write a handler** — a procedure that takes a hash table of arguments and returns a string:

{lang="scheme", linenos=off}
```
(def (my-lookup-handler args)
  (let ((query (or (hash-get args 'query) "")))
    (string-append "Result for: " query)))
```

2. **Register it** in a registry with a JSON-schema describing its parameters:

{lang="scheme", linenos=off}
```
(register-tool! my-registry
  "my_lookup"
  "Look up information about a topic"
  (list->hash-table
    `(("type" . "object")
      ("properties" .
       ,(list->hash-table
          `(("query" .
             ,(list->hash-table
                `(("type" . "string")
                  ("description" . "The search query")))))))
      ("required" . ("query"))))
  my-lookup-handler)
```

3. **Call ollama-with-tools** with your registry:

{lang="scheme", linenos=off}
```
(ollama-with-tools "Look up information about Gerbil Scheme"
                   my-registry)
```

The model will read the tool descriptions you provide and decide when to use them based on the user's prompt. Good tool descriptions are important — they help the model understand when a tool is appropriate and what arguments to pass.
