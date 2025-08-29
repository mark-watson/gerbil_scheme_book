# Inexpensive and Fast LLM Inference Using the Groq Service

Dear reader, are you excited about integrating LLMs into your applications but you want to miniize costs?

Groq is rapidly making a name for itself in the AI community as a cloud-based large language model (LLM) inference provider, distinguished by its revolutionary hardware and remarkably low-cost, high-speed performance. At the heart of Groq's impressive capabilities lies its custom-designed Language Processing Unit (LPU), a departure from the conventional GPUs that have long dominated the AI hardware landscape. Unlike GPUs, which are general-purpose processors, the LPU is an application-specific integrated circuit (ASIC) meticulously engineered for the singular task of executing LLM inference. This specialization allows for a deterministic and streamlined computational process, eliminating many of the bottlenecks inherent in more versatile hardware. The LPU's architecture prioritizes memory bandwidth and minimizes latency, enabling it to process and generate text at a blistering pace, often an order of magnitude faster than its GPU counterparts. This focus on inference, the process of using a trained model to make predictions, positions Groq as a compelling solution for real-time applications where speed is paramount.

The practical implications of Groq's technological innovation are multifaceted, offering a potent combination of affordability, speed, and a diverse selection of open-source models. The efficiency of the LPU translates directly into a more cost-effective pricing structure for users, with a pay-as-you-go model based on the number of tokens processed. This transparent and often significantly cheaper pricing democratizes access to powerful AI, enabling developers and businesses of all sizes to leverage state-of-the-art models without prohibitive upfront costs. The platform's raw speed is a game-changer, facilitating near-instantaneous responses that are crucial for interactive applications like chatbots, content generation tools, and real-time data analysis. Furthermore, Groq's commitment to the open-source community is evident in its extensive library of available models, including popular choices like Meta's Llama series, Mistral's Mixtral, and Google's Gemma. This wide array of options provides users with the flexibility to select the model that best suits their specific needs, all while benefiting from the unparalleled inference speeds and economic advantages offered by Groq's unique hardware.

## Structure of Project and Build Instructions

This project is stored in the directory **gerbil_scheme_book/source_code/groq_llm_inference**. Tere is one common utility file **groq_inference.ss** and currently two very short example scripts that use this utility:

- kimi2.ss - Uses Moonshot AI's Kimi2 model (MOE 1 trillion paramters, with 32B active).
- gpt-oss-120b.ss - Uses OpenAI's open source model gpt-oss-120b.

Both of these models are practical models that are excellent for data manipulation, coding, and general purpose use.

It's important to note that both models leverage a Mixture of Experts (MoE) architecture. This is a significant departure from traditional "dense" transformer models where every parameter is activated for every input token. In an MoE model, a "router" network selectively activates a small subset of "expert" sub-networks for each token, allowing for a massive total parameter count while keeping the computational cost for inference relatively low. The comparison, therefore, is between two different implementations and philosophies of the MoE approach.

Here is the project Makefile:

```makefile
compile: groq_inference.ss
	gxc groq_inference.ss 

kimi2: compile
	gxi -l kimi2.ss -

gpt-oss-120b: compile
	gxi -l gpt-oss-120b.ss -
```


### Kimi2 (Moonshot AI)

Features:

- Architecture: A very large-scale Mixture of Experts (MoE) model.
- Parameters: It has a staggering 1 trillion total parameters. For any given token during inference, it activates approximately 32 billion of these parameters. This represents a very sparse activation (around 3.2%).
- Specialization: Kimi2 is highly optimized for agentic capabilities, meaning it excels at using tools, reasoning through multi-step problems, and advanced code synthesis.
- Training Innovation: It was trained using a novel optimizer called MuonClip, designed to ensure stability during large-scale MoE training runs, which have historically been prone to instability.
- Context Window: It supports a large context window of up to 128,000 tokens, making it suitable for tasks involving long documents or extensive codebases.
- Licensing: While the model weights are publicly available ("open-weight"), its specific licensing and training data details are proprietary to Moonshot AI.

### gpt-oss-120b (OpenAI)

Features:

- Architecture: Also a Mixture of Experts (MoE) model, but at a smaller scale than Kimi2.
- Parameters: It has a total of 117 billion parameters, with a much smaller active set of around 5.1 billion parameters per token. This results in a similarly sparse activation (around 4.4%).
- Efficiency and Accessibility: A primary feature is its optimization for efficient deployment. It's designed to run on a single 80 GB GPU (like an H100), making it significantly more accessible for researchers and smaller organizations.
- Focus: Like Kimi2, it is designed for high-reasoning, agentic tasks, and general-purpose use.
- Licensing: It is a true open-source model, released under the permissive Apache 2.0 license. This allows for broad use, modification, and redistribution.
- Training: It was trained using a combination of reinforcement learning and distillation techniques from OpenAI's more advanced proprietary models.

### Comparison and Use Cases

| Feature | Kimi2 (Moonshot AI) | gpt-oss-120b (OpenAI) |
|---|---|---|
| **Architecture** | Massive-scale Mixture of Experts (MoE) | Efficient Mixture of Experts (MoE) |
| **Total Parameters** | ~1 Trillion | ~117 Billion |
| **Active Parameters** | ~32 Billion | ~5.1 Billion |
| **Primary Goal** | Pushing the upper limits of performance and scale. | Balancing high performance with deployment efficiency. |
| **Hardware Target** | Large-scale, high-end compute clusters. | Single high-end GPU (e.g., H100). |
| **Licensing** | Open-Weight (proprietary) | Open-Source (Apache 2.0) |
| **Key Differentiator** | Sheer scale; novel MuonClip optimizer. | Accessibility, efficiency, and permissive open license. |

## groq_inference.ss Utility

Here we construct a practical, reusable Gerbil Scheme function for interacting with the Groq API, a service renowned for its high-speed large language model inference. The function, named groq_inference, encapsulates the entire process of making a call to Groq's OpenAI-compatible chat completions endpoint. It demonstrates essential real-world programming patterns, such as making authenticated HTTP POST requests, dynamically building a complex JSON payload from Scheme data structures, and securely managing credentials using environment variables. This example not only provides a useful utility for integrating AI into your applications but also serves as an excellent case study in using Gerbil's standard libraries for networking (:std/net/request) and data interchange (:std/text/json), complete with robust error handling for both network issues and malformed API responses.

```scheme
(import :std/net/request
        :std/text/json)

(export groq_inference)

;; Generic Groq chat completion helper
;; Usage: (groq_inference model prompt [system-prompt: "..."])
(def (groq_inference
      model prompt
      system-prompt: (system-prompt "You are a helpful assistant."))
  (let ((api-key (get-environment-variable "GROQ_API_KEY")))
    (unless api-key
      (error "GROQ_API_KEY environment variable not set."))

    (let* ((headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(string-append "Bearer " api-key))))
           (body-data
            (list->hash-table
             `(("model" . ,model)
               ("messages"
                .
                ,(list
                  (list->hash-table `(("role" . "system") ("content" . ,system-prompt)))
                  (list->hash-table `(("role" . "user") ("content" . ,prompt))))))))
           (body-string (json-object->string body-data))
           (endpoint "https://api.groq.com/openai/v1/chat/completions"))
      
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
          (let* ((response-json (request-json response))
                 (choices (hash-ref response-json 'choices))
                 (first-choice (and (pair? choices) (car choices)))
                 (message (and first-choice (hash-ref first-choice 'message)))
                 (content (and message (hash-ref message 'content))))
            (or content (error "Groq response missing content")))
          (error "Groq API request failed"
            status: (request-status response)
            body: (request-text response)))))))
```

he implementation begins by defining the **groq_inference** function, which accepts a model and a prompt, along with an optional keyword argument for a system message. Its first action is a crucial security and configuration check: it attempts to fetch the GROQ_API_KEY from the environment variables, raising an immediate error if it's not found. The core of the function then uses a let* block to sequentially build the components of the HTTP request. It constructs the authorization headers and then assembles the JSON body using a combination of quasiquotation and the **list->hash-table** procedure to create the nested structure required by the API. This body is then serialized into a JSON string, and finally, the http-post function is called with the endpoint, headers, and data to execute the network request.

Upon receiving a response, the function demonstrates robust result processing and error handling. It first checks if the HTTP status code is 200 (OK), indicating a successful request. If it is, a series of **let*** bindings are used to safely parse the JSON response and navigate the nested data structure to extract the final content string from **response['choices'][0]['message']['content']**, with checks at each step to prevent errors on an unexpected response format. If the content is successfully extracted, it is returned as the result of the function. However, if the HTTP status is anything other than 200, the function enters its error-handling branch, raising a descriptive error that includes the failing status code and the raw text body of the response, providing valuable debugging information to the caller.

## Example scripts: kimi2.ss and gpt-oss-120b.ss

These two scripts are simple enough to just list without comment:

**kimi2.ss**

```scheme
(import :groq/groq_inference)

;; Use Moonshot AI's best kimi2 model (MOE: 1 triliion parameters, 32B resident).

; Export the `kimi2` procedure from this module
(export kimi2)

(def (kimi2 prompt
            model: (model "moonshotai/kimi-k2-instruct")
            system-prompt: (system-prompt "You are a helpful assistant."))
  (groq_inference model prompt system-prompt: system-prompt))

;; (kimi2 "why is the sky blue? be very concise")
```

**gpt-oss-120b.ss**

```scheme
(import :groq/groq_inference)

;; Use OpenAI's open source model gpt-oss-120b

; Export the `gpt-oss-120b` procedure from this module
(export gpt-oss-120b)

(def (gpt-oss-120b
      prompt
      model: (model "openai/gpt-oss-120b")
      system-prompt: (system-prompt "You are a helpful assistant."))
  (groq_inference model prompt system-prompt: system-prompt))

;; (gpt-oss-120b "why is the sky blue? be very concise")
```

Running the kimi2 exaple:

Note, the utility must be comiled one time: **gxc groq_inference.ss**. The compiled library by default will be in the directory **~/.gerbil/lib/groq/** because we set this project's module name to **groq** in the file **gerbil.pkg**.

```console
 $ gxi -l kimi2.ss                   
> (displayln (kimi2 "explain concisely what evidence there is for 'dark matter' in the universe, and counter arguments. Be concise!"))
Evidence for dark matter  
• Galaxy rotation curves: outer stars orbit too fast for visible mass alone.  
• Gravitational lensing: mass maps exceed baryonic matter.  
• Cosmic Microwave Background: tiny temperature ripples fit models with ~5× more dark than baryonic matter.  
• Structure formation: simulations need unseen matter to match today’s galaxy distribution.  
• Bullet Cluster: collision separated hot gas (baryons) from dominant mass peak, consistent with collisionless dark matter.

Counter-arguments / alternatives  
• Modified Newtonian Dynamics (MOND): tweaks gravity law to explain rotation curves without extra mass.  
• Modified gravity theories (TeVeS, f(R), emergent gravity) reproduce lensing and CMB with no dark particles.  
• Claims of inconsistent lensing signals or tidal dwarf galaxies without dark matter challenge universality.
> 
```

Running the gpt-oss-120b example:

```console
$ gxi -l gpt-oss-120b.ss
> (displayln (gpt-oss-120b "write a recursive Haskell function 'factorial'. Only show the code."))
``haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
``
>
```

