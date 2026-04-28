---
name: gerbil-expert
description: Gerbil Scheme tutorial, idioms, and API reference for all examples in Mark Watson's Gerbil Scheme book "Artificial Intelligence Algorithms in Gerbil Scheme". Use this skill for writing Gerbil Scheme code that accesses LLMs (Gemini, OpenAI, Ollama, Groq), SPARQL/Wikidata queries, NLP, RDF data stores, FFI bindings, and more.
---

# Notes for Using AGENT Skills with Gerbil Scheme Book Examples

This document helps readers set up coding agent skills so that AI assistants can reference the Gerbil Scheme APIs and patterns from this book when generating code.

## Source code for Gemini, OpenAI, Ollama, Groq, SPARQL/Wikidata, NLP, RDF example code

```bash
git clone https://github.com/markwatson/gerbil_scheme_book.git
```

All the Gerbil Scheme examples are in the `source_code/` directory.  Look in ~/GITHUB/gerbil_scheme_book/source_code/ for code to reuse.

---

## Gerbil Scheme Tutorial and Idioms

Gerbil Scheme is an opinionated dialect of Scheme built on the Gambit runtime. It provides an advanced module system, first-class actors, an expressive macro system (syntax-case), and native FFI — all compiling to efficient C via the Gambit backend.

### Core Syntax

```scheme
;; Printing
(displayln "Hello from Gerbil!")

;; Variable binding (module-level)
(def x 42)
(def name "Mark")

;; Local binding
(let ((x 10)
      (y 20))
  (+ x y))   ; => 30

;; Arithmetic (prefix notation)
(+ 1 2 3)       ; => 6
(* 2 (+ 3 4))   ; => 14
```

### Imports and Exports

```scheme
;; Import standard library modules
(import :std/net/request
        :std/text/json
        :std/format)

;; Import a local module (relative path, no .ss extension)
(import "example_tools" "use_tools")

;; Import a package module (uses gerbil.pkg)
(import :nlp/main :nlp/utils)

;; Export specific symbols
(export gemini ollama-with-tools)

;; Export all symbols
(export #t)
```

### Functions

```scheme
;; Define a function (Gerbil-style)
(def (greet name)
  (string-append "Hello, " name "!"))

;; Optional / keyword arguments
(def (gemini prompt
             model: (model "gemini-2.5-flash")
             system-prompt: (system-prompt "You are a helpful assistant."))
  ...)

;; Anonymous functions
(map (lambda (x) (* x x)) '(1 2 3 4 5))  ; => (1 4 9 16 25)

;; Traditional define also works
(define (process-file fpath)
  (process-string (file->string fpath)))
```

### Control Flow

```scheme
;; if (two branches)
(if (= x 1)
  (displayln "one")
  (displayln "not one"))

;; when (single branch, no else)
(when (> x 0)
  (displayln "positive"))

;; unless
(unless api-key
  (error "API key not set."))

;; cond (multiple branches)
(cond
  ((< x 0) (displayln "negative"))
  ((= x 0) (displayln "zero"))
  (else    (displayln "positive")))

;; begin (group multiple expressions)
(if condition
  (begin
    (displayln "a")
    (displayln "b"))
  (displayln "else"))
```

### Loops and Iteration

```scheme
;; for-each
(for-each (lambda (item) (displayln item)) '("a" "b" "c"))

;; map
(map (lambda (x) (* x x)) '(1 2 3))   ; => (1 4 9)

;; do loop
(do ((i 0 (+ i 1)))
    ((= i 10))
  (display i))

;; for (Gerbil's :std/iter)
(import :std/iter)
(for (n (in-range 5))
  (displayln n))

;; Named let (tail-recursive loop)
(let loop ((n 10) (acc 1))
  (if (= n 0) acc
      (loop (- n 1) (* acc n))))
```

### Data Structures

```scheme
;; Lists
(def fruits '("apple" "banana" "cherry"))
(car fruits)            ; => "apple"
(cdr fruits)            ; => ("banana" "cherry")
(cons "date" fruits)    ; => ("date" "apple" "banana" "cherry")

;; Hash tables
(def config (make-hash-table))
(hash-put! config "host" "localhost")
(hash-ref config "host")             ; => "localhost"
(hash-get config "missing")          ; => #f (no error)

;; Quick hash from alist
(def ht (list->hash-table '(("a" . 1) ("b" . 2))))

;; Vectors
(def v (vector 1 2 3))
(vector-ref v 0)        ; => 1

;; Gambit tables (legacy, still used in NLP code)
(def tbl (make-table))
(table-set! tbl "key" "value")
(table-ref tbl "key" #f)
```

### String Operations

```scheme
;; String append
(string-append "Hello" ", " "world")

;; Format (from :std/format)
(import :std/format)
(format "Hello, ~a!" name)        ; => "Hello, Mark!"

;; String predicates
(string-prefix? "Hello" "Hello, world")
(string-suffix? "ed" "walked")
```

### Exception Handling

```scheme
;; with-catch
(with-catch
  (lambda (e) (displayln "Error: " e))
  (lambda () (/ 10 0)))

;; error
(error "Something failed" key: value)
```

### JSON

```scheme
(import :std/text/json)

;; Build JSON object
(def body (list->hash-table
            `(("model" . "gemini-2.5-flash")
              ("prompt" . "Hello")
              ("stream" . #f))))

;; Serialize to string
(json-object->string body)

;; Parse from response
(request-json response)     ; returns hash table
(hash-ref ht 'key)          ; access parsed JSON fields (symbols)
```

### HTTP Requests

```scheme
(import :std/net/request)

;; GET
(let ((response (http-get url headers: headers)))
  (if (= (request-status response) 200)
      (request-json response)
      (error "Request failed")))

;; POST with JSON body
(let ((response (http-post endpoint headers: headers data: body-string)))
  (request-status response)    ; => 200
  (request-text response)      ; => raw body string
  (request-json response))     ; => parsed hash table
```

### FFI (Foreign Function Interface)

```scheme
(import :std/foreign)

;; Minimal FFI binding
(begin-ffi (rdf-init rdf-query rdf-free)
  (c-declare "
    int   rdf_init(const char*);
    char* rdf_query_copy(const char*);
    void  rdf_free(void);
  ")
  (define-c-lambda rdf-init  (char-string) int         "rdf_init")
  (define-c-lambda rdf-query (char-string) char-string "rdf_query_copy")
  (define-c-lambda rdf-free  ()            void        "rdf_free"))
```

### Module System and Packages

```scheme
;; gerbil.pkg — declares a package namespace
;; Example content: (package: nlp)

;; Build script
(import :std/build-script)
(defbuild-script
  '("command_line_utilities/lib"
    (exe: "command_line_utilities/main" bin: "command_line_utilities")))
```

### Quasiquote and Alist Construction

```scheme
;; Quasiquote for building nested data
(let ((model "gemini-2.5-flash")
      (prompt "Hello"))
  `(("model" . ,model)
    ("prompt" . ,prompt)
    ("stream" . #f)))

;; list->hash-table with quasiquoted alist
(list->hash-table
  `(("role" . "user")
    ("content" . ,prompt)))
```

---

# Gerbil Scheme Book APIs — Quick Reference

Knowledge of public APIs and usage patterns for the Gerbil Scheme examples in Mark Watson's book *Artificial Intelligence Algorithms in Gerbil Scheme*.

## Project Setup

Each example directory typically contains a `Makefile` and a `gerbil.pkg` file. Run examples interactively in `gxi` (the Gerbil REPL) or compile via `gxc`:

```bash
cd source_code/<example_name>
make                 # compile (if Makefile provided)
gxi <script>.ss      # run interactively
```

---

## hello

**Directory:** `hello/`
**Deps:** None (stdlib only)

### API

- `(hello who)` — Prints "hello" followed by the given name.

### Example

```scheme
(import "hello")
(hello 'Brady)   ; => hello Brady
```

---

## gemini

**Directory:** `gemini/`
**Deps:** `:std/net/request`, `:std/text/json`
**Env var:** `GOOGLE_API_KEY`
**Model:** `gemini-2.5-flash`

### API

- `(gemini prompt model: "gemini-2.5-flash" system-prompt: "You are a helpful assistant.")` — Send a prompt to the Google Gemini API. Returns the response text string.

### Example

```scheme
(import "gemini")
(gemini "why is the sky blue? be very concise")
```

---

## openai

**Directory:** `openai/`
**Deps:** `:std/net/request`, `:std/text/json`
**Env var:** `OPENAI_API_KEY`
**Model:** `gpt-5-mini`

### API

- `(openai prompt model: "gpt-5-mini" system-prompt: "You are a helpful assistant.")` — Send a chat completion request to the OpenAI API. Returns the response text string.

### Example

```scheme
(import "openai")
(openai "why is the sky blue? be very concise")
```

---

## ollama

**Directory:** `ollama/`
**Deps:** `:std/net/request`, `:std/text/json`
**Server:** Requires Ollama running locally on port 11434
**Model:** `gemma3:latest` (default)

### Scripts and APIs

- **`ollama.ss`** — Simple text generation via `/api/generate`.
  - `(ollama prompt model: "gemma3:latest")` — Returns the response text string.

- **`example_tools.ss`** — Tool/function-calling registry for Ollama:
  - `(make-tool-registry)` — Create an empty tool registry.
  - `(register-tool! registry name description parameters handler)` — Register a tool.
  - `(tool-registry-lookup registry name)` — Look up a tool by name.
  - `(tool-registry-schemas registry)` — Get JSON schemas for all registered tools.
  - `default-tool-registry` — Pre-built registry with `get_weather` and `calculate` tools.
  - `(get-weather-handler args)` — Handler for weather lookups.
  - `(calculate-handler args)` — Handler for math expression evaluation.

- **`use_tools.ss`** — Agent loop for tool/function calling via `/api/chat`:
  - `(ollama-chat messages tools: #f model: "qwen3:1.7b")` — Single chat request with optional tool schemas.
  - `(ollama-with-tools prompt registry model: "qwen3:1.7b" max-rounds: 5)` — Full agent loop: sends prompt, executes tool calls, feeds results back until final answer.

### Examples

```scheme
;; Simple generation
(import "ollama")
(ollama "why is the sky blue? Be very concise.")

;; Tool calling
(import "example_tools" "use_tools")
(ollama-with-tools "What's the weather like in Paris?"
                   default-tool-registry)

(ollama-with-tools "What is 42 * 17?"
                   default-tool-registry)
```

---

## groq_llm_inference

**Directory:** `groq_llm_inference/`
**Deps:** `:std/net/request`, `:std/text/json`
**Env var:** `GROQ_API_KEY`

### Scripts and APIs

- **`groq_inference.ss`** — Generic Groq chat completion helper:
  - `(groq_inference model prompt system-prompt: "You are a helpful assistant.")` — Returns the response text string.

- **`gpt-oss-120b.ss`** — Convenience wrapper for OpenAI's open-source `gpt-oss-120b` model:
  - `(gpt-oss-120b prompt)` — Returns the response text string.

- **`kimi2.ss`** — Convenience wrapper for Moonshot AI's Kimi K2 model:
  - `(kimi2 prompt)` — Returns the response text string.

### Examples

```scheme
;; Generic Groq call
(import :groq/groq_inference)
(groq_inference "openai/gpt-oss-120b" "why is the sky blue? be very concise")

;; Using the wrapper
(import :groq/gpt-oss-120b)
(gpt-oss-120b "why is the sky blue? be very concise")

(import :groq/kimi2)
(kimi2 "why is the sky blue? be very concise")
```

---

## NLP

**Directory:** `NLP/`
**Deps:** `:std/iter`
**Build:** `make` (compiles via `build.ss`)

A native Gerbil NLP toolkit including part-of-speech tagging, key phrase extraction, text categorization, human name recognition, and text summarization — no external dependencies required.

### Key Modules

- **`main.ss`** — Main processing pipeline:
  - `(process-file fpath)` — Process a text file. Returns `(words tags key-phrases categories summary-words proper-names place-names)`.
  - `(process-string str)` — Process a text string. Same return format.

- **`nlp.ss`** — Command-line interface and JSON output:
  - `(json-write ret)` — Write NLP results as JSON.
  - `(main . argv)` — CLI entry point: `-i <input> -o <output>`.

- **`fasttag.ss`** — Part-of-speech tagger:
  - `(parts-of-speech words)` — Tag a vector of words. Returns a vector of POS tags.

- **`utils.ss`** — Text utilities:
  - `(words-from-string str)` — Tokenize a string into a vector of words.
  - `(noise-word? word)` — Check if a word is a stop word.
  - `(no-noise words)` — Remove stop words from a word vector.

- **`proper-names.ss`** — Name extraction:
  - `(find-human-names words acc)` — Extract human names from a word vector.

- **`category.ss`** — Text categorization:
  - `categoryHashtables` / `categoryNames` — Pre-loaded category data.

- **`key-phrases.ss`** — Key phrase extraction:
  - `(key-phrases input-file-path)` — Extract 1/2/3-word key phrases. Returns `(1-grams 2-grams 3-grams)`.

### Example

```scheme
(import :nlp/main)
(let ((result (process-file "data/testdata/climate_g8.txt")))
  (displayln "Words: " (car result))
  (displayln "Tags: " (cadr result))
  (displayln "Key phrases: " (caddr result))
  (displayln "Categories: " (cadddr result)))
```

---

## command_line_NLP

**Directory:** `command_line_NLP/`
**Deps:** `:nlp/main`, `:openai/openai`, `:std/misc/ports`

Command-line tools that pipe stdin or argv text through NLP or LLM APIs.

### Scripts

- **`summarize.ss`** — Summarize text using OpenAI:
  - `(main . args)` — Reads stdin or argv, sends to OpenAI for summarization.

- **`categories.ss`** — Categorize text using the native NLP engine:
  - `(main . args)` — Reads stdin or argv, runs through `process-string`, prints categories.

### Example

```bash
echo "Climate change affects global policy." | gxi summarize.ss
echo "Banking in Europe is a growing business." | gxi categories.ss
```

---

## command_line_utilities

**Directory:** `command_line_utilities/`
**Deps:** `:std/build-script`, `:std/cli/getopt`, `:std/cli/print-exit`, `:std/format`

### Build

Uses `defbuild-script` to compile a standalone executable:

```bash
cd command_line_utilities
gxi build.ss
```

---

## command_line_utilities_first_demo_START_HERE

**Directory:** `command_line_utilities_first_demo_START_HERE/`
**Deps:** `:std/cli/getopt`, `:std/cli/print-exit`, `:std/format`

Introductory example showing Gerbil's CLI argument parsing with `getopt`.

### API

- `(main . argv)` — Parses `--name=STR` and optional `-v` flag.

### Example

```bash
gxi test-tool.ss --name="World"
gxi test-tool.ss -v --name="Gerbil"
```

---

## wikidata

**Directory:** `wikidata/`
**Deps:** `:std/net/request`, `:std/text/json`, `:std/net/uri`, `:std/format`

### API

- `(query-wikidata sparql-query [user-agent])` — Execute a SPARQL query against the Wikidata Query Service. Returns a list of hash tables (one per result row).
- `(query-wikidata/alist sparql-query [user-agent])` — Same as above but returns association lists per row: `((var . value) ...)`.
- `(query-dbpedia ...)` — Backward-compatibility alias for `query-wikidata`.
- `(alist-ref key row [default])` — Look up a key in an alist result row.

### Example

```scheme
(import "wikidata")

;; Query birth date of Grace Hopper
(let ((results (query-wikidata/alist
                 (string-append
                   "PREFIX wd: <http://www.wikidata.org/entity/>\n"
                   "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
                   "SELECT ?birthDate WHERE {\n"
                   "  wd:Q7249 wdt:P569 ?birthDate .\n"
                   "}"))))
  (for-each (lambda (r)
              (displayln "Birth date: " (alist-ref 'birthDate r)))
            results))
```

---

## RDF_datastore

**Directory:** `RDF_datastore/`
**Deps:** None (stdlib only)

A pure Gerbil in-memory RDF triple store with a toy SPARQL SELECT parser and evaluator.

### API

- `(make-store)` — Create an empty triple store.
- `(add-triple st s p o)` — Add a triple to the store.
- `(remove-triple st s p o)` — Remove a matching triple.
- `(store-triples st)` — Retrieve all triples.
- `(sparql-select st query)` — Run a SELECT query. Returns a list of alists mapping variable names to values.
- `(print-all-triples st)` — Display all triples for debugging.

### Example

```scheme
(import "RDF")

(let ((st (make-store)))
  (add-triple st "<ex:alice>" "<foaf:name>" "\"Alice\"")
  (add-triple st "<ex:alice>" "<foaf:knows>" "<ex:bob>")
  (add-triple st "<ex:bob>" "<foaf:name>" "\"Bob\"")

  (displayln (sparql-select st
    "SELECT ?name WHERE { <ex:alice> <foaf:knows> ?friend . ?friend <foaf:name> ?name . }")))
```

---

## SparqlRdfStore

**Directory:** `SparqlRdfStore/`
**Deps:** `:std/foreign`, `libRDFWrap.dylib` (C library)
**Platform:** macOS (uses `.dylib`); Linux variant available

FFI bindings to a C-based RDF store library via Gerbil's `:std/foreign`.

### API

- `(rdf-init filename)` — Initialize the RDF store from a file. Returns 0 on success.
- `(rdf-query sparql-string)` — Execute a SPARQL query. Returns result as a string.
- `(rdf-free)` — Free the RDF store resources.

### Example

```scheme
(import "rdfwrap")
(rdf-init "data.nt")
(displayln (rdf-query "select ?s ?p ?o { ?s ?p ?o }"))
(rdf-free)
```

---

## RaptorRDF_FFI

**Directory:** `RaptorRDF_FFI/`
**Deps:** `:std/foreign`, `libraptor2` (system library)
**Platform:** macOS (uses Raptor2 C library)

FFI bindings to the Raptor2 RDF parsing library for converting RDF files to N-Triples.

### API

- `(raptor-parse-file->ntriples filename syntax-name)` — Parse an RDF file with the given syntax (e.g., `"turtle"`, `"rdfxml"`, `"guess"`) and return all triples as an N-Triples string.

### Example

```scheme
(import "ffi")
(displayln (raptor-parse-file->ntriples "data.ttl" "turtle"))
```

---

## General Notes

- All examples use Gerbil Scheme's standard library (`:std/...`) for HTTP, JSON, URI encoding, FFI, iterators, and CLI parsing.
- Run examples interactively with `gxi` or compile with `gxc`.
- Each module directory typically has a `gerbil.pkg` file defining its package namespace.
- Environment variables must be set before use: `GOOGLE_API_KEY`, `OPENAI_API_KEY`, `GROQ_API_KEY`.
- Ollama requires a local server running on `http://localhost:11434`.
- Gerbil uses `def` (Gerbil-style) and `define` (R7RS-style) interchangeably for function/variable definitions.
- Use `hash-ref` for fields that must exist (raises error on missing), `hash-get` for optional fields (returns `#f`).
- Use `list->hash-table` with quasiquoted alists to build JSON-like structures.
- Use keyword arguments (`key: (key default)`) for optional parameters in Gerbil-style `def`.
- The NLP module is self-contained — no external NLP libraries required.
