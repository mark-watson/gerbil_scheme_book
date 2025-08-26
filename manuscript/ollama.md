# Ollama

Ollama is a powerful and user-friendly tool designed to simplify the process of running large language models (LLMs) locally on personal hardware. In a landscape often dominated by cloud-based APIs, Ollama democratizes access to advanced AI by providing a simple command-line interface that bundles model weights, configurations, and a tailored execution environment into a single, easy-to-install package. It allows developers, researchers, and enthusiasts to download and interact with a wide range of popular open-source models, such as Llama 3, Mistral, and Phi-3, with just a single command. Beyond its interactive chat functionality, Ollama also exposes a local REST API, enabling the seamless integration of these locally-run models into custom applications without the latency, cost, or privacy concerns associated with remote services. This focus on accessibility and local deployment makes it an indispensable tool for offline development, rapid prototyping, and leveraging the power of modern LLMs while maintaining full control over data and infrastructure.

TBD

## Example Code

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

A few comments: in the second example I added "Just output the bash script and nothing else." to the end of the prompt. Without this, the model will generate a 100 lines of design notes, instructions how to make the bash script executable, etc. I didn't want that, just the bash script.

In the third example, I used the same prompt but used **displayln** to print the result in a more useful format.
