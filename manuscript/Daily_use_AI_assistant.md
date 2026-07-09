# Building a Personal AI Assistant CLI with Gerbil Scheme

## The Idea: Your Own AI Terminal Companion

Modern AI language models are extraordinarily capable, but they are stateless. Each conversation starts fresh; the model has no memory of what you discussed yesterday, last week, or even five minutes ago in a different session. When you use these models day after day for real work such as researching a problem, tracking a project, exploring a technology then you constantly repeat yourself, re-establishing context that the model promptly forgets.

This chapter builds a solution to that problem: a command-line REPL (Read-Eval-Print Loop) that wraps the Google Gemini API with a persistent, file-based context cache. The tool learns from your sessions. When you save a useful answer, that knowledge becomes available as injected context in future queries and the result is a simple but effective form of Retrieval-Augmented Generation (RAG) you can run entirely from a terminal.

The project also demonstrates a key Gerbil Scheme strength: building self-contained, production-quality command-line tools with minimal dependencies, using only the standard library for HTTP, JSON, and module compilation.

## Theory: Grounding, RAG, and the Context Window

Large language models operate within a fixed *context window* that defines the maximum amount of text they can process in a single interaction. Everything the model needs to know must fit inside that window. For a daily-use assistant, this creates a tension: each new query arrives in isolation, stripped of accumulated knowledge.

### Google Search Grounding

One solution is to give the model access to real-time information retrieval. The Gemini API supports *search grounding*: instead of relying solely on trained knowledge, Gemini can issue a Google Search query, read the results, and synthesize an answer. This is essential for questions about current events, prices, showtimes, or anything that changes after the model's training cutoff.

### Retrieval-Augmented Generation

RAG is the general technique of augmenting a model's prompt with retrieved documents or passages. The retrieval step selects only the context relevant to the current query, staying within the context window while giving the model access to a much larger body of knowledge than the window alone could hold.

This project implements a lightweight RAG pipeline:

1. The user saves an answer to a persistent file-based store.
2. When a new query arrives, keywords are extracted from the query.
3. Those keywords are matched against stored entries using bag-of-words overlap.
4. Matching entries are prepended to the prompt as context before the query is sent to Gemini.

The key insight is that even a simple keyword-matching retriever, without embeddings or vector search, provides meaningful context injection for a single user's personal query history. The topics you ask about repeatedly are exactly the ones whose keywords will match.

### Bag-of-Words Keyword Matching

Bag-of-words matching ignores word order and treats a document as a multiset of words. Given a query like "what sci-fi movies are playing in Flagstaff", the retriever strips stop words (common words like "what", "are", "in") to get content words: `["sci-fi", "movies", "playing", "flagstaff"]`. Any cached entry containing at least one of these words is returned as context.

This is not semantic search, it cannot match synonyms or related concepts, but for personal use it is surprisingly effective. Your own notes tend to use the same vocabulary as your questions.

## Project Structure

The project compiles to a single binary, `daily-use`. Two source files live in the `daily_use/` package directory:

```
daily_use/
├── build.ss          # Compilation script
├── gerbil.pkg        # Package declaration
├── Makefile          # Build convenience wrapper
└── daily_use/
    ├── cache.ss      # Persistent cache module
    └── main.ss       # REPL and Gemini API client
```

The `gerbil.pkg` file declares the package namespace:

```scheme
(package: daily-use)
```

The `build.ss` script tells the Gerbil build system what to compile and where to put the resulting binary:

```scheme
#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("daily_use/cache"
    (exe: "daily_use/main" bin: "daily-use")))
```

The `defbuild-script` form compiles `cache.ss` as a library module first, then compiles `main.ss` as the executable entry point, linking everything into a standalone binary named `daily-use`.

## The Cache Module

The cache is the heart of the persistent context system. It stores answers as S-expression pairs in a plain text file, one per line.

### Cache File Format

The cache file lives at `~/.daily-use-cache-gerbil.db`. Each line is a Scheme datum:

```scheme
(1748149200 . "For today, Sunday, May 24, 2026, the following science fiction
movie is playing in Flagstaff, AZ: Project Hail Mary (PG-13)...")
(1748235600 . "Gerbil Scheme is a production-quality Scheme implementation
built on top of the Gambit runtime...")
(1748322000 . "The Gemini Interactions API supports multi-turn conversations
and tool use via the /v1beta/interactions endpoint...")
```

Each entry is a *dotted pair*: a cons cell whose `car` is a Unix timestamp (seconds since the epoch) and whose `cdr` is the answer text. Gerbil's built-in `read` and `write` procedures handle serialization and deserialization transparently, with no custom parser needed.

### Complete Cache Source

Here is the full `cache.ss` module:

```scheme
;;; -*- Gerbil -*-
;;; daily_use/cache.ss — File-based persistent cache for Gemini answers
;;;
;;; Stores entries as s-expression pairs in a file, one per line:
;;;   (timestamp . "answer text")
;;;
;;; Provides add, lookup (bag-of-words matching), count, clear, and
;;; clear-older-than-one-week operations.

(import :std/sugar :std/error)
(export #t)

(def (make-cache path)
  (let* ((entries (load-cache-entries path))
         (ht (make-hash-table)))
    (hash-put! ht 'path path)
    (hash-put! ht 'entries entries)
    ht))

(def (load-cache-entries path)
  (if (file-exists? path)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (call-with-input-file path
          (lambda (port)
            (let lp ((items '()))
              (let ((item (read port)))
                (if (eof-object? item)
                  (reverse items)
                  (lp (cons item items)))))))))
    '()))

(def (save-cache-entries cache)
  (call-with-output-file (hash-ref cache 'path)
    (lambda (port)
      (for-each (lambda (entry)
                  (write entry port)
                  (newline port))
                (hash-ref cache 'entries)))))

(def (cache-add cache text)
  (let ((entry (cons (time->seconds (current-time)) text)))
    (hash-put! cache 'entries
               (append (hash-ref cache 'entries) (list entry)))
    (save-cache-entries cache)))

(def (cache-count cache)
  (length (hash-ref cache 'entries)))

(def (cache-lookup cache keywords (limit 10))
  (let* ((entries (hash-ref cache 'entries))
         (matching
           (filter (lambda (entry)
                     (any (lambda (kw)
                            (string-contains-ci? (cdr entry) kw))
                          keywords))
                   entries)))
    (map cdr (take matching limit))))

(def (cache-clear cache)
  (hash-put! cache 'entries '())
  (save-cache-entries cache))

(def (cache-clear-older-one-week cache)
  (let* ((one-week-ago (- (time->seconds (current-time)) (* 7 24 60 60)))
         (entries (hash-ref cache 'entries))
         (kept (filter (lambda (entry)
                         (>= (car entry) one-week-ago))
                       entries)))
    (hash-put! cache 'entries kept)
    (save-cache-entries cache)))

(def (cache-close cache)
  (save-cache-entries cache))

(def (take lst n)
  (let lp ((lst lst) (n n) (acc '()))
    (if (or (null? lst) (<= n 0))
      (reverse acc)
      (lp (cdr lst) (- n 1) (cons (car lst) acc)))))

(def (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(def (string-contains-ci? haystack needle)
  (let ((hl (string-length haystack))
        (nl (string-length needle)))
    (let lp ((i 0))
      (if (> (+ i nl) hl)
        #f
        (if (string-ci=? (substring haystack i (+ i nl)) needle)
          #t
          (lp (+ i 1)))))))
```

### Walking Through the Cache Module

**`make-cache`** creates the cache as a hash table with two keys: `path` (where to persist the file) and `entries` (the in-memory list of cons pairs). Using a hash table as a record-like structure is idiomatic Gerbil when a `defstruct` would be heavier than needed.

**`load-cache-entries`** reads the file line by line using Scheme's standard `read` procedure. Notice the `with-catch` wrapper: if the file is malformed or unreadable, the lambda returns an empty list rather than crashing the REPL on startup. The named `let lp` loop accumulates items in reverse (for efficiency), then `reverse`es them at the end.

**`save-cache-entries`** rewrites the entire file on every modification. For hundreds of entries this is perfectly fast; it avoids the complexity of append-only formats or in-place editing. Each entry is written with `write`, which produces machine-readable Scheme datums (strings are quoted, special characters are escaped), followed by a newline to separate entries.

**`cache-add`** captures the current time with `(time->seconds (current-time))` that is a Gambit runtime call that returns the number of seconds since the Unix epoch as an integer. It appends the new entry to the list and immediately persists.

**`cache-lookup`** is where the RAG retrieval happens. It filters all entries to those containing any of the query keywords (case-insensitively), then returns up to `limit` matching texts. The `any` helper short-circuits: as soon as one keyword matches, the entry is included.

**`string-contains-ci?`** implements a simple sliding-window substring search with `string-ci=?` for case-insensitive comparison. There is no external string library needed.

**`cache-clear-older-one-week`** computes the cutoff timestamp as `(current-time-seconds - 7×24×60×60)` and filters the entry list with `>=`. The arithmetic `(* 7 24 60 60)` evaluates to 604,800 seconds.

## The Main REPL Module

The main module provides the Gemini API client, the keyword extraction pipeline, and the interactive REPL loop.

### Complete Main Source

```scheme
;;; -*- Gerbil -*-
;;; daily_use/main.ss — Interactive Gemini REPL with search grounding and cache

(import :std/net/request
        :std/text/json
        :std/sugar
        ./cache)

(export main)

(def *model* "gemini-3-flash-preview")

(def *cache* #f)
(def *last-answer* #f)

(def *stop-words*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "do" "does" "did" "will" "would" "shall" "should"
    "may" "might" "must" "can" "could" "am" "it" "its"
    "in" "on" "at" "to" "for" "of" "with" "by" "from" "as"
    "and" "or" "but" "not" "no" "nor" "so" "yet"
    "this" "that" "these" "those" "what" "which" "who" "whom"
    "i" "me" "my" "we" "our" "you" "your" "he" "she" "they" "them"
    "how" "when" "where" "why" "if" "then" "than" "about"))

(def (extract-keywords text)
  (let* ((downcased (string-downcase text))
         (words (string-split downcased))
         (cleaned (map (lambda (w)
                         (string-trim-punctuation w))
                       words)))
    (filter (lambda (w)
              (and (> (string-length w) 2)
                   (not (member w *stop-words*))))
            cleaned)))

(def (build-context-from-cache query)
  (let* ((keywords (extract-keywords query))
         (items (if (pair? keywords)
                   (cache-lookup *cache* keywords 10)
                   '())))
    (if (pair? items)
      (let ((ctx "Use the following context from previous conversations when answering:\n\n"))
        (for-each (lambda (item)
                    (set! ctx (string-append ctx "- " item "\n")))
                  items)
        (string-append ctx "---\n\n"))
      "")))

(def (%gemini-post body-data)
  (let ((api-key (getenv "GOOGLE_API_KEY")))
    (unless api-key
      (error "GOOGLE_API_KEY environment variable not set."))
    (let* ((headers `(("Content-Type" . "application/json")
                      ("x-goog-api-key" . ,api-key)
                      ("Api-Revision" . "2026-05-20")))
           (body-string (json-object->string body-data))
           (endpoint (string-append
                      "https://generativelanguage.googleapis.com/v1beta/interactions?key="
                      api-key))
           (response (http-post endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
        (let* ((response-json (request-json response))
               (steps (hash-ref response-json 'steps))
               (model-step
                 (let lp ((ss (reverse steps)))
                   (cond
                     ((null? ss) #f)
                     ((equal? (hash-ref (car ss) 'type) "model_output") (car ss))
                     (else (lp (cdr ss))))))
               (content (hash-ref model-step 'content))
               (first-content (car content)))
          (hash-ref first-content 'text))
        (error "Gemini API request failed, status: "
               (request-status response))))))

(def (ask-gemini prompt search-p: (search-p #f))
  (let ((context (build-context-from-cache prompt)))
    (let ((full-prompt (string-append context prompt)))
      (with-catch
        (lambda (e)
          (string-append "[Error calling Gemini API: " e "]"))
        (lambda ()
          (if search-p
            (let* ((search-tool (list->hash-table '(("type" . "google_search"))))
                   (body-data
                     (list->hash-table
                       `(("model" . ,*model*)
                         ("input" . ,full-prompt)
                         ("tools" . ,(list search-tool))))))
              (%gemini-post body-data))
            (let ((body-data
                    (list->hash-table
                      `(("model" . ,*model*)
                        ("input" . ,full-prompt)))))
              (%gemini-post body-data))))))))

(def (print-help)
  (displayln "")
  (displayln "  Gemini Daily-Use REPL")
  (displayln "  ─────────────────────────────────────────")
  (displayln "  <text>         Ask Gemini a question")
  (displayln "  !<text>        Ask with Google Search grounding")
  (displayln "  >              Add last answer to cache")
  (displayln "  !              Clear cache entries older than 1 week")
  (displayln "  h / help       Show this help")
  (displayln "  q / quit       Exit")
  (displayln "  Ctrl-D         Exit")
  (displayln "  ─────────────────────────────────────────")
  (displayln "  Model: " *model*)
  (displayln "  Cache: ~/.daily-use-cache-gerbil.db (" (cache-count *cache*) " items)")
  (displayln ""))

(def (display-answer text)
  (if text
    (begin
      (displayln "")
      (displayln text)
      (displayln "")
      (set! *last-answer* text))
    (displayln "\n  [No response from Gemini — check model name or API key]\n")))

(def (repl-loop)
  (displayln "\n  Gemini Daily-Use REPL  (type 'h' for help)\n")
  (let lp ()
    (display "gemini> ")
    (let ((input (read-line)))
      (if (eof-object? input)
        (displayln "\nGoodbye.")
        (let ((trimmed (string-trim input)))
          (cond
            ((string=? trimmed "")
             (lp))

            ((member trimmed '("q" "quit" "exit"))
             (displayln "Goodbye."))

            ((member trimmed '("h" "help"))
             (print-help)
             (lp))

            ((string=? trimmed ">")
             (if *last-answer*
               (begin
                 (cache-add *cache* *last-answer*)
                 (displayln "  [Cached. " (cache-count *cache*) " items total]"))
               (displayln "  [No answer to cache yet]"))
             (lp))

            ((string=? trimmed "!")
             (let ((before (cache-count *cache*)))
               (cache-clear-older-one-week *cache*)
               (let ((after (cache-count *cache*)))
                 (displayln "  [Cleared " (- before after)
                            " old entries. " after " items remain]")))
             (lp))

            ((string-prefix? "!" trimmed)
             (let ((query (string-trim (substring trimmed 1
                                                  (string-length trimmed)))))
               (if (string=? query "")
                 (let ((before (cache-count *cache*)))
                   (cache-clear-older-one-week *cache*)
                   (let ((after (cache-count *cache*)))
                     (displayln "  [Cleared " (- before after)
                                " old entries. " after " items remain]")))
                 (begin
                   (displayln "  [Searching...]")
                   (force-output)
                   (display-answer (ask-gemini query search-p: #t)))))
             (lp))

            (else
              (displayln "  [Thinking...]")
              (force-output)
              (display-answer (ask-gemini trimmed))
              (lp))))))))

(def (main . args)
  (let ((cache-path (string-append (getenv "HOME" "")
                                   "/.daily-use-cache-gerbil.db")))
    (set! *cache* (make-cache cache-path))
    (set! *last-answer* #f)
    (repl-loop)
    (cache-close *cache*)
    (displayln "  [Cache closed]")))
```

### Walking Through the Main Module

#### Global State

Three module-level variables manage shared state. In Gerbil, `def` at the top level creates a module binding; the `*earmuff*` naming convention signals that a variable is mutable global state:

```scheme
(def *model* "gemini-3-flash-preview")
(def *cache* #f)        ; set to cache hash-table in main
(def *last-answer* #f)  ; set to string after each successful query
```

Using `#f` as the initial value for uninitialized state is idiomatic Gerbil. The `main` entry point initializes both before entering the REPL.

#### Keyword Extraction Pipeline

`extract-keywords` chains three transformations:

```scheme
(def (extract-keywords text)
  (let* ((downcased (string-downcase text))
         (words (string-split downcased))
         (cleaned (map (lambda (w)
                         (string-trim-punctuation w))
                       words)))
    (filter (lambda (w)
              (and (> (string-length w) 2)
                   (not (member w *stop-words*))))
            cleaned)))
```

1. **Lowercase** the entire query is converted to lower case for case-insensitive matching.
2. **Split** on whitespace into individual tokens using the custom `string-split` helper.
3. **Strip punctuation** from each token's leading and trailing characters.
4. **Filter**: keep only words longer than two characters that do not appear in the stop-word list.

The use of the `let*` form (sequential binding) is essential here: each binding can reference the previous one, which a plain `let` would not allow.

#### The Gemini API Client

The `%gemini-post` function (prefixed with `%` to signal it is a private internal function) handles the raw HTTP interaction with the Gemini Interactions API:

```scheme
(def (%gemini-post body-data)
  (let ((api-key (getenv "GOOGLE_API_KEY")))
    (unless api-key
      (error "GOOGLE_API_KEY environment variable not set."))
    (let* ((headers `(("Content-Type" . "application/json")
                      ("x-goog-api-key" . ,api-key)
                      ("Api-Revision" . "2026-05-20")))
           ...
           (response (http-post endpoint headers: headers data: body-string)))
      ...)))
```

The headers use a quasiquoted association list. The backtick (`` ` ``) introduces a template, and commas (`,`) splice runtime values into fixed positions. The `x-goog-api-key` header and `Api-Revision` header authenticate and version the request.

The response from the Interactions API has a `steps` array. Each step has a `type` field; the text answer lives in the last step whose type is `"model_output"`. The code searches from the end by reversing the steps list and walking forward:

```scheme
(let lp ((ss (reverse steps)))
  (cond
    ((null? ss) #f)
    ((equal? (hash-ref (car ss) 'type) "model_output") (car ss))
    (else (lp (cdr ss)))))
```

This named-`let` loop is Gerbil's idiomatic tail-recursive iteration. The label `lp` is local to the `let` form; calling `(lp next-args)` is a tail call that does not grow the stack.

#### Search Grounding vs. Plain Queries

`ask-gemini` selects between two JSON body shapes based on the `search-p:` keyword argument:

```scheme
;; With search grounding:
(list->hash-table
  `(("model" . ,*model*)
    ("input" . ,full-prompt)
    ("tools" . ,(list search-tool))))

;; Without:
(list->hash-table
  `(("model" . ,*model*)
    ("input" . ,full-prompt)))
```

The `search-tool` is itself a hash table `{"type": "google_search"}`. Including it in the `tools` array instructs Gemini to perform a live web search before answering.

#### The REPL Loop

`repl-loop` is a classic read-dispatch-print loop, implemented with a named `let lp` that calls itself at the end of each non-exiting branch:

```scheme
(def (repl-loop)
  (displayln "\n  Gemini Daily-Use REPL  (type 'h' for help)\n")
  (let lp ()
    (display "gemini> ")
    (let ((input (read-line)))
      (if (eof-object? input)
        (displayln "\nGoodbye.")
        (let ((trimmed (string-trim input)))
          (cond
            ((string=? trimmed "") (lp))
            ((member trimmed '("q" "quit" "exit")) (displayln "Goodbye."))
            ((member trimmed '("h" "help")) (print-help) (lp))
            ((string=? trimmed ">") ... (lp))
            ((string=? trimmed "!") ... (lp))
            ((string-prefix? "!" trimmed) ... (lp))
            (else (display-answer (ask-gemini trimmed)) (lp))))))))
```

The `(force-output)` calls before displaying `[Thinking...]` or `[Searching...]` ensure the status message appears immediately, before the blocking HTTP request begins. Without `force-output`, Gerbil's I/O buffering might hold the message until after the response arrives.

`(eof-object? input)` handles Ctrl-D cleanly so when standard input is closed, `read-line` returns the EOF object rather than a string.

#### String Utility Functions

Rather than importing a string library, the module defines three focused helpers:

- **`string-trim`**: strips leading and trailing spaces and tabs using two scanning loops (one from the left, one from the right).
- **`string-trim-punctuation`**: the same pattern but for a specific set of punctuation characters using `memv` (member with `eqv?`).
- **`string-split`**: splits on whitespace by iterating character-by-character, accumulating a current word and consing it onto a result list when a space or tab is seen.

All three use the same `let lp` idiom with explicit index arithmetic, keeping the implementation transparent and dependency-free.

## Building and Running

### Prerequisites

- Gerbil Scheme v0.18 or later (`brew install gerbil-scheme` on macOS)
- A Google API key with the Generative Language API enabled
- The `GOOGLE_API_KEY` environment variable set

### Build

```bash
cd daily_use
export GOOGLE_API_KEY=your-key-here

# Install dependencies and compile
make build

# Or directly with gxpkg:
gxpkg deps -i
gxpkg build
```

The compiled binary is placed at `.gerbil/bin/daily-use`.

### Install System-Wide

```bash
make install
# binary is now at /usr/local/bin/daily-use
```

### Run

```bash
daily-use
# or directly from build output:
.gerbil/bin/daily-use
```

## Sample Session

Here is an annotated session demonstrating the key features:

```
$ daily-use

  Gemini Daily-Use REPL  (type 'h' for help)

gemini> h

  Gemini Daily-Use REPL
  ─────────────────────────────────────────
  <text>         Ask Gemini a question
  !<text>        Ask with Google Search grounding
  >              Add last answer to cache
  !              Clear cache entries older than 1 week
  h / help       Show this help
  q / quit       Exit
  Ctrl-D         Exit
  ─────────────────────────────────────────
  Model: gemini-3-flash-preview
  Cache: ~/.daily-use-cache-gerbil.db (0 items)

gemini> !what sci-fi movies are playing today in Flagstaff AZ?
  [Searching...]

For today, Sunday, July 9, 2026, the following science fiction movies
are playing in Flagstaff, AZ:

*   **Dune: Messiah** (PG-13) is showing at the **Harkins Flagstaff 16**
    at 2:15pm, 5:00pm, and 8:30pm.
*   **Project Hail Mary** (PG-13) is also showing at the **Harkins
    Flagstaff 16** at 1:30pm, 4:45pm, and 7:50pm.

Please check the Harkins Theatres website or your preferred ticketing
platform to confirm current showtimes.

gemini> >
  [Cached. 1 items total]

gemini> what are some good sci-fi movies playing near me?
  [Thinking...]

Based on our previous conversation, **Dune: Messiah** and **Project
Hail Mary** are currently playing at the Harkins Flagstaff 16. Both
are critically acclaimed — Project Hail Mary in particular has received
outstanding reviews for its faithful adaptation of Andy Weir's novel.

gemini> !current version of Gerbil Scheme as of today?
  [Searching...]

As of July 2026, the current stable release of Gerbil Scheme is
**v0.18.1**, released in May 2026. It includes improvements to the
module system, updated Gambit runtime support, and enhanced FFI
documentation.

gemini> >
  [Cached. 2 items total]

gemini> what gerbil version should I install?
  [Thinking...]

Based on our previous conversation, the current stable release of
Gerbil Scheme is **v0.18.1** (as of July 2026). That is the version
you should install — it can be obtained via `brew install
gerbil-scheme` on macOS.

gemini> !
  [Cleared 0 old entries. 2 items remain]

gemini> q
Goodbye.
  [Cache closed]
```

## Interpreting the Results

### Search vs. No-Search Responses

The first query (`!what sci-fi movies...`) uses Google Search grounding. Notice that the response includes specific showtimes and current release titles. The model has gone out to the web and synthesized real information.

The third query (`what are some good sci-fi movies playing near me?`) uses no `!` prefix so it is a plain query. The model's response accurately references the movies from the earlier search-grounded answer, even though this query sent no search request. The cache injection is working: keywords like `sci-fi`, `movies`, `playing`, and `flagstaff` matched the stored entry, which was prepended to the prompt.

This is the core benefit of the RAG loop: the second query gets the right answer without paying for a second search round-trip, because the relevant context was injected from the local cache.

### Cache as Accumulated Personal Knowledge

After several sessions, the cache file might contain dozens or hundreds of entries covering topics you frequently ask about: library versions, API endpoints, meeting notes, technical explanations, local information. Each subsequent query that touches those topics automatically pulls in relevant context, making Gemini behave more like an assistant that knows your history.

### The `!` Maintenance Command

The bare `!` command purges entries older than seven days. This is not just housekeeping: it prevents the cache from growing unbounded, and removes stale information (yesterday's movie listings, last week's flight status) that would pollute context for future queries. The timestamp stored in each entry's `car` makes the cutoff calculation exact and efficient.

### Performance Characteristics

The cache is read entirely into memory at startup. For a cache with hundreds of entries, this takes milliseconds. The `cache-lookup` function scans every entry with `filter`, so lookup time is linear in the number of entries which is acceptable for personal use scales, where thousands of entries would still complete in under a millisecond on modern hardware. If the cache grew to tens of thousands of entries, an inverted index mapping keywords to entries would be the natural next step.

## Wrap Up

This chapter built a complete personal AI assistant CLI in Gerbil Scheme. The project demonstrates several patterns that recur throughout practical Gerbil development:

**S-expressions as a data format.** The cache file uses Scheme's native `read`/`write` for serialization, with no custom parser and no external library. The language's homoiconic nature turns structured persistence into a two-function operation.

**Hash tables as lightweight records.** Rather than defining a struct for the cache object, a hash table with known symbol keys (`'path`, `'entries`) serves the same purpose with less boilerplate. This is appropriate when the structure is internal to a module and never crosses API boundaries.

**Named `let` for all loops.** Whether walking a list, scanning a string, or running the REPL itself, Gerbil expresses iteration as a tail-recursive named `let`. There are no `while` or `for` loops and the language encourages a single, uniform recursion idiom that is easy to reason about and guaranteed tail-call optimized.

**`with-catch` for resilience at boundaries.** Wrapping the file load and the HTTP call in `with-catch` makes the tool resilient to the two most common real-world failures: a missing or corrupt cache file at startup, and a network error during an API call. The error handlers return safe fallback values rather than crashing.

**`defbuild-script` for standalone binaries.** Gerbil's build system compiles a multi-file project to a single self-contained binary with one build command. No Makefile complexity beyond a thin wrapper is needed.

The tool as presented is immediately useful and intentionally minimal. The next natural extensions would be multi-turn conversation history (sending previous turns along with the current prompt), a vector-embedding retriever to replace the bag-of-words keyword search, or support for multiple LLM backends using the OpenAI or Groq modules shown elsewhere in this book.

---

## Practice Problems

These exercises extend the concepts from this chapter. Each problem is designed to be completed in Gerbil Scheme, building on the `cache.ss` and `main.ss` modules.

**Problem 1: Cache Statistics Command**

Add a `?` command to the REPL that displays cache statistics: total entry count, the date of the oldest entry, the date of the newest entry, and the total number of characters stored across all entries. Update the `repl-loop` dispatch `cond` and add a `print-cache-stats` helper function in `main.ss`.

*Hint:* Use `(car entry)` to get the timestamp as seconds, then convert to a human-readable date using Gambit's `time` procedures.

**Problem 2: Keyword Highlighting in Cache Lookup**

Modify `build-context-from-cache` so that when it prepends cached entries to the prompt, it also prints a line to the terminal showing which keywords matched which entries:

```
  [Cache hit: "flagstaff", "movies" matched 1 entry]
```

The print should go to the terminal (standard output) but should not be included in the prompt sent to Gemini.

**Problem 3: Named Slots**

Extend the cache to support named entries. Add a `/save name` command (e.g., `/save gerbil-version`) that saves the last answer under a specific label, and a `/get name` command that retrieves it by name and prints it without querying Gemini. The named entries should be stored alongside regular timestamped entries in the same file, using a distinct format:

```scheme
(name "gerbil-version" . "As of July 2026, the current stable release is v0.18.1...")
```

Update `load-cache-entries` and `save-cache-entries` to handle both entry formats.

**Problem 4: Configurable Stop-Word List**

The stop-word list is currently hard-coded in `main.ss`. Move it to a configuration file at `~/.daily-use-stopwords.txt`, one word per line. Load it at startup in `main` and fall back to the default list if the file does not exist. Add a `+word` REPL command to add a word to the active stop-word list and write it to the file.

**Problem 5: Ollama Backend**

Add support for a local Ollama backend. When the REPL is started with a `--ollama` flag and optionally a `--model=MODEL` flag, route queries to `http://localhost:11434/api/generate` instead of the Gemini API. The cache and context injection should work identically regardless of backend. Implement a `%ollama-post` function analogous to `%gemini-post`, and refactor `ask-gemini` into a generic `ask-llm` function that dispatches based on a module-level backend variable.

*Hint:* Study the Ollama module in `source_code/ollama/` for the correct JSON request structure.

**Problem 6: Cache Export**

Add an `export` REPL command that writes all cache entries to a human-readable Markdown file at `~/daily-use-export-YYYYMMDD.md`. Each entry should appear as a Markdown section with the timestamp as a heading and the answer text as the body. The file should be usable as a personal knowledge base document.
