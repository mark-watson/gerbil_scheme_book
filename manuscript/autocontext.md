# AutoContext: Hybrid Retrieval-Augmented Generation (RAG)

Large language models excel at synthesizing information, but they operate within finite context windows and lack access to private, local document collections. When building an application that must query a library of reference texts, research papers, or local notes, you cannot paste thousands of pages directly into a single prompt. Doing so is either impossible due to token limits or prohibitively expensive and slow.

Retrieval-Augmented Generation (RAG) solves this constraint. Instead of sending the full text corpus to the model, an indexing pipeline first divides documents into manageable chunks. When a user submits a query, a retrieval engine scans the index, identifies the most relevant passages, and constructs an augmented prompt containing only those excerpts. The language model then synthesizes a grounded answer from the retrieved evidence.

Many production RAG systems depend on external vector databases and third-party embedding services. While dense neural embeddings capture semantic similarities, they frequently fail on rare proper nouns, acronyms, and specialized jargon. Conversely, traditional sparse keyword search algorithms excel at finding specific terms but miss synonymous phrasing. 

The AutoContext system presented in this chapter combines both approaches in pure Gerbil Scheme:

1. **BM25 (Okapi)**: a probabilistic ranking function that scores documents based on term frequency, inverse document frequency, and document length normalization.
2. **TF-IDF Vector Space**: a vector space model that computes term weights and scores candidate passages using cosine similarity.
3. **Hybrid Merger and Deduplicator**: an aggregator that queries both indexes, unifies the candidate passages by document chunk index, and eliminates duplicates.
4. **Prompt Generator and LLM Bridge**: a formatter that packages the retrieved context into a prompt and optionally delegates the final answer generation to a local Ollama model.

## Theoretical Foundations of Hybrid Retrieval

To understand how AutoContext retrieves the best passages, we examine the mathematics behind BM25 and TF-IDF cosine similarity.

### The BM25 (Okapi) Scoring Function

BM25 is a ranking algorithm widely used in information retrieval. Given a query `Q`$ containing terms `q_1, q_2, \dots, q_n`$ and a candidate document `D`$, the BM25 score is computed as:

```$
\mathrm{score}(D, Q) = \sum_{i=1}^n \mathrm{IDF}(q_i) \cdot \frac{f(q_i, D) \cdot (k_1 + 1)}{f(q_i, D) + k_1 \cdot \left(1 - b + b \cdot \frac{|D|}{\mathrm{avgdl}}\right)}
```

In this formula:
- `f(q_i, D)`$ is the term frequency of query word `q_i`$ within document `D`$.
- `|D|`$ is the length of document `D`$ in tokens.
- `\mathrm{avgdl}`$ is the average document length across the entire corpus.
- `k_1`$ is a calibration parameter that controls term frequency saturation limits (typically set to 1.5).
- `b`$ is a parameter that controls the degree of document length normalization (typically set to 0.75).

The Inverse Document Frequency (IDF) penalizes words that occur across many documents while rewarding rare, informative terms:

```$
\mathrm{IDF}(q_i) = \log_{10}\left(\frac{N - n(q_i) + 0.5}{n(q_i) + 0.5}\right)
```

Here `N`$ represents the total number of documents in the corpus, and `n(q_i)`$ is the number of documents containing term `q_i`$.

### TF-IDF and Cosine Distance

While BM25 estimates relevance probabilistically, the Vector Space Model represents documents and queries as high-dimensional vectors. Each dimension corresponds to a unique term in the vocabulary.

The weight `w_{t, D}`$ of term `t`$ in document `D`$ is defined by combining sub-linear term frequency scaling with smoothed inverse document frequency:

```$
w_{t, D} = (1 + \log(f(t, D))) \cdot \left(1 + \log\left(\frac{1 + N}{1 + n(t)}\right)\right)
```

The similarity between a query vector `\mathbf{q}`$ and a document vector `\mathbf{d}`$ is the cosine of the angle between them:

```$
\mathrm{cosine\_similarity}(\mathbf{q}, \mathbf{d}) = \frac{\mathbf{q} \cdot \mathbf{d}}{\|\mathbf{q}\| \|\mathbf{d}\|} = \frac{\sum_{t} w_{t, Q} \cdot w_{t, D}}{\sqrt{\sum_{t} w_{t, Q}^2} \sqrt{\sum_{t} w_{t, D}^2}}
```

Because sparse vectors for short text passages contain non-zero weights for only a tiny fraction of the vocabulary, hash tables provide an ideal data structure for sparse dot-product calculations.

## Input Data Representation

AutoContext processes plain text files located in a designated data directory. Before indexing, let us look at a typical sample passage from `data/economics.txt`:

```text
There exists an economic problem, subject to study by economic science,
when a decision (choice) is made by one or more resource-controlling players
to attain the best possible outcome under bounded rational conditions...

An interesting Economist is Pauli Blendergast who teaches at the University
of Krampton Ohio and is famouse for saying economics is bullshit.
```

The text contains both formal academic discussion and an idiosyncratic assertion about an economist named Pauli Blendergast. A query like `"who says that economics is bullshit?"` demands an exact keyword hit on rare terms like `"bullshit"` and `"Blendergast"`, alongside contextual background on economics.

## Implementing BM25 Okapi in Gerbil Scheme

The module `bm25.ss` implements the complete BM25 ranking function. It defines a record type `bm25-index-struct` to store document frequencies, document lengths, average document length, and per-document term frequency tables.

```scheme
;; File: bm25.ss
;; A self-contained implementation of the BM25 (Okapi) ranking function.
;;
;; For a query Q and document D:
;;
;;   score(D, Q) = sum over terms t in Q of
;;       IDF(t) * tf(t,D) * (k1 + 1) / (tf(t,D) + k1 * (1 - b + b * |D| / avgdl))
;;
;;   IDF(t) = log10((N - df(t) + 0.5) / (df(t) + 0.5))

(import :std/sort)
(export make-bm25-index bm25-index? bm25-score bm25-top-n)

;; corpus: vector of token lists; term-freqs: vector of hash tables (term -> count)
(defstruct bm25-index-struct (doc-freqs doc-lengths avg-doc-length corpus-size term-freqs k1 b))

(def bm25-index? bm25-index-struct?)
(def bm25-index-doc-freqs bm25-index-struct-doc-freqs)
(def bm25-index-doc-lengths bm25-index-struct-doc-lengths)
(def bm25-index-avg-doc-length bm25-index-struct-avg-doc-length)
(def bm25-index-corpus-size bm25-index-struct-corpus-size)
(def bm25-index-term-freqs bm25-index-struct-term-freqs)
(def bm25-index-k1 bm25-index-struct-k1)
(def bm25-index-b bm25-index-struct-b)

(def (count-terms tokens)
  (let ((ht (make-hash-table)))
    (for-each (lambda (tok) (hash-update! ht tok 1+ 0)) tokens)
    ht))

;; Build a BM25 index from a list of tokenized documents (lists of strings).
(def (make-bm25-index tokenized-corpus k1: (k1 1.5) b: (b 0.75))
  (let* ((corpus-size (length tokenized-corpus))
         (doc-lengths (list->vector (map length tokenized-corpus)))
         (avg-doc-length (/ (foldl + 0 (vector->list doc-lengths))
                            (exact->inexact (max corpus-size 1))))
         (term-freqs (list->vector (map count-terms tokenized-corpus)))
         (doc-freqs (make-hash-table)))
    ;; document frequency: number of documents containing each term
    (vector-for-each (lambda (tf)
                       (for-each (lambda (term) (hash-update! doc-freqs term 1+ 0))
                                 (hash-keys tf)))
                     term-freqs)
    (make-bm25-index-struct doc-freqs doc-lengths avg-doc-length corpus-size term-freqs k1 b)))

(def (idf index term)
  (let ((df (hash-ref (bm25-index-doc-freqs index) term 0))
        (n (bm25-index-corpus-size index)))
    (/ (log (/ (+ (- n df) 0.5) (+ df 0.5))) (log 10))))

;; BM25 score of document DOC-INDEX for the list QUERY-TOKENS.
(def (bm25-score index query-tokens doc-index)
  (let ((k1 (bm25-index-k1 index))
        (b (bm25-index-b index))
        (dl (vector-ref (bm25-index-doc-lengths index) doc-index))
        (avgdl (bm25-index-avg-doc-length index))
        (tf-table (vector-ref (bm25-index-term-freqs index) doc-index)))
    (foldl (lambda (term score)
             (let ((tf (hash-ref tf-table term 0)))
               (+ score
                  (* (idf index term)
                     (/ (* tf (+ k1 1))
                        (+ tf (* k1 (+ (- 1 b) (* b (/ dl avgdl))))))))))
           0.0
           query-tokens)))

;; Return up to N (score . doc-index) pairs, best first. Returning the index
;; lets callers map hits back to the original text and merge with other
;; retrievers by identity.
(def (bm25-top-n index query-tokens n)
  (let* ((scores (map (lambda (i) (cons (bm25-score index query-tokens i) i))
                      (iota (bm25-index-corpus-size index))))
         (sorted (sort scores (lambda (a b) (> (car a) (car b))))))
    (take-up-to sorted n)))

(def (take-up-to lst n)
  (if (or (null? lst) (<= n 0)) '() (cons (car lst) (take-up-to (cdr lst) (- n 1)))))
```

Notice that `bm25-top-n` returns pairs of `(score . doc-index)`. Tracking candidate documents by integer index allows downstream code to deduplicate results when combining multiple ranking methods.

## Implementing Vector-Space TF-IDF in Gerbil Scheme

The module `tfidf.ss` provides the vector space companion retriever. Each document chunk is converted into a sparse vector represented as a hash table from term to numeric weight.

```scheme
;; File: tfidf.ss
;; A simple vector-space retriever: TF-IDF weighted sparse vectors compared
;; with cosine similarity.

(import :std/sort)
(export make-tfidf-index tfidf-index? tfidf-vector cosine-similarity tfidf-top-n)

;; idf: term -> inverse document frequency; vectors: vector of sparse vectors
(defstruct tfidf-index-struct (idf vectors))
(def tfidf-index? tfidf-index-struct?)
(def tfidf-index-idf tfidf-index-struct-idf)
(def tfidf-index-vectors tfidf-index-struct-vectors)

;; A sparse vector is a hash table mapping term -> weight.
(def (tfidf-vector idf tokens)
  (let ((v (make-hash-table)))
    (for-each (lambda (tok) (hash-update! v tok 1+ 0)) tokens)
    (hash-for-each (lambda (term tf)
                     (hash-put! v term (* (+ 1.0 (log tf)) (hash-ref idf term 0.0))))
                   v)
    v))

(def (make-tfidf-index tokenized-corpus)
  (let* ((n (length tokenized-corpus))
         (df (make-hash-table))
         (idf (make-hash-table)))
    (for-each (lambda (doc)
                (let ((seen (make-hash-table)))
                  (for-each (lambda (tok)
                              (unless (hash-get seen tok)
                                (hash-put! seen tok #t)
                                (hash-update! df tok 1+ 0)))
                            doc)))
              tokenized-corpus)
    ;; smoothed idf, always positive
    (hash-for-each (lambda (term d) (hash-put! idf term (+ 1.0 (log (/ (+ 1.0 n) (+ 1.0 d))))))
                   df)
    (make-tfidf-index-struct idf (list->vector (map (lambda (doc) (tfidf-vector idf doc))
                                                    tokenized-corpus)))))

(def (norm v)
  (sqrt (hash-fold (lambda (k w acc) (+ acc (* w w))) 0.0 v)))

(def (cosine-similarity a b)
  (let ((na (norm a)) (nb (norm b)))
    (if (or (zero? na) (zero? nb))
      0.0
      (/ (hash-fold (lambda (term w acc) (+ acc (* w (hash-ref b term 0.0)))) 0.0 a)
         (* na nb)))))

;; Return up to N (similarity . doc-index) pairs, best first.
(def (tfidf-top-n index query-tokens n)
  (let* ((q (tfidf-vector (tfidf-index-idf index) query-tokens))
         (vectors (tfidf-index-vectors index))
         (sims (map (lambda (i) (cons (cosine-similarity q (vector-ref vectors i)) i))
                    (iota (vector-length vectors))))
         (sorted (sort sims (lambda (a b) (> (car a) (car b))))))
    (let loop ((l sorted) (k n) (acc '()))
      (if (or (null? l) (= k 0)) (reverse acc) (loop (cdr l) (- k 1) (cons (car l) acc))))))
```

Using `hash-fold` computes the sparse vector dot product efficiently in `O(min(|A|, |B|))` operations without allocating dense vectors for unused vocabulary items.

## Orchestrating the AutoContext Pipeline

The `autocontext.ss` module ties the indexing, sentence chunking, hybrid retrieval, prompt formatting, and Ollama HTTP integration together.

```scheme
;; File: autocontext.ss
;; AutoContext: build effective one-shot LLM prompts from a directory of text
;; files that is too large to paste into a single context window.

(import :std/format :std/sort :std/misc/ports :std/net/request :std/text/json
        "bm25" "tfidf")
(export make-auto-context auto-context-chunks get-prompt
        ask-with-context ollama-generate tokenize chunk-text
        split-into-sentences load-and-chunk-documents)

(defstruct auto-context-data (chunks bm25 tfidf))
(def auto-context? auto-context-data?)
(def auto-context-chunks auto-context-data-chunks)
(def auto-context-bm25 auto-context-data-bm25)
(def auto-context-tfidf auto-context-data-tfidf)

;;; ------------------------------------------------------------------------
;;; Text processing
;;; ------------------------------------------------------------------------

(def stop-words
  (let ((ht (make-hash-table)))
    (for-each (lambda (w) (hash-put! ht w #t))
              '("a" "an" "the" "is" "are" "was" "were" "of" "to" "in" "and" "or"
                "that" "this" "it" "for" "on" "with" "as" "by" "be" "at" "who"
                "what" "which" "how" "does" "do" "from" "its"))
    ht))

;; Lower-case, split on non-alphanumeric characters, and drop stop words.
(def (tokenize text)
  (let loop ((chars (string->list (string-downcase text))) (cur '()) (acc '()))
    (define (flush)
      (if (null? cur)
        acc
        (let ((w (list->string (reverse cur))))
          (if (hash-get stop-words w) acc (cons w acc)))))
    (cond
     ((null? chars) (reverse (flush)))
     ((or (char-alphabetic? (car chars)) (char-numeric? (car chars)))
      (loop (cdr chars) (cons (car chars) cur) acc))
     (else (loop (cdr chars) '() (flush))))))

(def (trim s)
  (let* ((n (string-length s))
         (start (let loop ((i 0)) (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i)))
         (end (let loop ((i n)) (if (and (> i start) (char-whitespace? (string-ref s (- i 1)))) (loop (- i 1)) i))))
    (substring s start end)))

;; Heuristic sentence splitter: break after . ? or ! when followed by
;; whitespace, a quote, or the end of the text.
(def (split-into-sentences text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
       ((= i n)
        (let ((last (trim (substring text start n))))
          (reverse (if (> (string-length last) 0) (cons last acc) acc))))
       ((and (memv (string-ref text i) '(#\. #\? #\!))
             (or (= (+ i 1) n)
                 (memv (string-ref text (+ i 1)) '(#\space #\newline #\"))))
        (let ((s (trim (substring text start (+ i 1)))))
          (loop (+ i 1) (+ i 1) (if (> (string-length s) 0) (cons s acc) acc))))
       (else (loop (+ i 1) start acc))))))

(def (join strings sep)
  (if (null? strings)
    ""
    (foldl (lambda (s acc) (string-append acc sep s)) (car strings) (cdr strings))))

;; Group sentences into chunks of CHUNK-SIZE sentences.
(def (chunk-text text chunk-size: (chunk-size 3))
  (let loop ((sentences (split-into-sentences text)) (acc '()))
    (if (null? sentences)
      (reverse acc)
      (let take ((s sentences) (k chunk-size) (group '()))
        (if (or (null? s) (= k 0))
          (loop s (cons (join (reverse group) " ") acc))
          (take (cdr s) (- k 1) (cons (car s) group)))))))

(def (txt-files-in-directory dir)
  (map (lambda (f) (path-expand f dir))
       (sort (filter (lambda (f) (string-suffix? ".txt" f)) (directory-files dir))
             string<?)))

(def (string-suffix? suffix s)
  (let ((ls (string-length s)) (lx (string-length suffix)))
    (and (>= ls lx) (string=? suffix (substring s (- ls lx) ls)))))

(def (load-and-chunk-documents dir)
  (let ((chunks (filter (lambda (s) (> (string-length s) 0))
                        (apply append
                               (map (lambda (f) (chunk-text (read-file-string f)))
                                    (txt-files-in-directory dir))))))
    (printf "Loaded ~a text chunks.\n" (length chunks))
    chunks))

;;; ------------------------------------------------------------------------
;;; Index construction and retrieval
;;; ------------------------------------------------------------------------

;; Load DIRECTORY-PATH and build both retrievers.
(def (make-auto-context directory-path)
  (printf "Initializing AutoContext from directory: ~a\n" directory-path)
  (let* ((chunks (list->vector (load-and-chunk-documents directory-path)))
         (tokenized (map tokenize (vector->list chunks))))
    (displayln "Building BM25 and TF-IDF retrievers...")
    (make-auto-context-data chunks (make-bm25-index tokenized) (make-tfidf-index tokenized))))

(def (delete-duplicates* lst)
  (let loop ((l lst) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

;; Retrieve context for QUERY and format it into a prompt for an LLM.
(def (get-prompt ac query num-results: (num-results 5))
  (printf "--- Retrieving context for query: '~a' ---\n" query)
  (let* ((q-tokens (tokenize query))
         ;; 1. sparse keyword search (ignore chunks with no matching terms)
         (bm25-idx (map cdr (filter (lambda (hit) (> (car hit) 0))
                                    (bm25-top-n (auto-context-bm25 ac) q-tokens num-results))))
         ;; 2. vector-space similarity search
         (vec-idx (map cdr (filter (lambda (hit) (> (car hit) 0))
                                   (tfidf-top-n (auto-context-tfidf ac) q-tokens num-results))))
         ;; 3. merge and deduplicate by chunk index
         (unique (delete-duplicates* (append bm25-idx vec-idx)))
         (context (map (lambda (i) (vector-ref (auto-context-chunks ac) i)) unique)))
    (printf "BM25 found ~a keyword-based results.\n" (length bm25-idx))
    (printf "TF-IDF search found ~a similarity-based results.\n" (length vec-idx))
    (printf "Combined and deduplicated, we have ~a context chunks.\n" (length context))
    ;; 4. format the final prompt
    (string-append
     "Based on the following context, please answer the question:\n" query
     "\n\n--- CONTEXT ---\n"
     (apply string-append (map (lambda (c) (string-append c "\n---\n")) context))
     "--- END CONTEXT ---\n\nQuestion: " query "\nAnswer:")))

;;; ------------------------------------------------------------------------
;;; Optional: answer with a local Ollama model
;;; ------------------------------------------------------------------------

(def (ollama-generate prompt model: (model "qwen3.5:4b"))
  (let* ((body (json-object->string
                (list->hash-table `(("model" . ,model) ("prompt" . ,prompt)
                                    ("stream" . #f) ("think" . #f)))))
         (response (http-post "http://localhost:11434/api/generate"
                              headers: '(("Content-Type" . "application/json"))
                              data: body)))
    (if (= (request-status response) 200)
      (hash-ref (request-json response) 'response)
      (error "Ollama API request failed" (request-status response) (request-text response)))))

;; Build a context prompt for QUESTION and have an Ollama model answer it.
(def (ask-with-context ac question model: (model "qwen3.5:4b") num-results: (num-results 5))
  (let ((prompt (get-prompt ac question num-results: num-results)))
    (printf "\n--- Sending ~a-character prompt to ~a ---\n" (string-length prompt) model)
    (ollama-generate prompt model: model)))
```

The deduplication step in `get-prompt` combines the top ranking chunks from both retrievers and filters out repeated identifiers. This guarantees that if a chunk scores highly under both BM25 and TF-IDF, it appears only once in the context block.

## Test Suite and Verification

The test driver `test.ss` loads the document repository, runs a query through the retrieval pipeline, builds the prompt, and attempts to call Ollama:

```scheme
;; File: test.ss
;; Demo for AutoContext: loads data/*.txt, retrieves context for a query,
;; and builds the prompt. If Ollama is available, optionally sends it.

(import :std/format
        "autocontext")

(def (run-test)
  (let* ((data-dir "data")
         (ac (make-auto-context data-dir))
         (query "who says that economics is bullshit?")
         (prompt (get-prompt ac query num-results: 2)))
    (displayln "\n=== Generated Prompt for LLM ===")
    (displayln prompt)

    ;; Test with Ollama if running
    (with-catch
     (lambda (e)
       (displayln "\n[Note: Local Ollama server not reachable or request failed, skipping LLM query]")
       (displayln "Prompt generation succeeded."))
     (lambda ()
       (displayln "\n=== Querying Ollama (qwen3.5:4b) ===")
       (let ((ans (ask-with-context ac query model: "qwen3.5:4b" num-results: 2)))
         (displayln "\n--- Answer from Ollama ---")
         (displayln ans))))))

(run-test)
```

## Running the Code and Expected Output

Execute the test suite using `make test`:

```console
$ make test
gxi test.ss
Initializing AutoContext from directory: data
Loaded 22 text chunks.
Building BM25 and TF-IDF retrievers...
--- Retrieving context for query: 'who says that economics is bullshit?' ---
BM25 found 2 keyword-based results.
TF-IDF search found 2 similarity-based results.
Combined and deduplicated, we have 2 context chunks.

=== Generated Prompt for LLM ===
Based on the following context, please answer the question:
who says that economics is bullshit?

--- CONTEXT ---
There exists an economic problem, subject to study by economic science, when a decision (choice) is made by one or more resource-controlling players to attain the best possible outcome under bounded rational conditions. In other words, resource-controlling agents maximize value subject to the constraints imposed by the information the agents have, their cognitive limitations, and the finite amount of time they have to make and execute a decision. Economic science centers on the activities of the economic agents that comprise society.[1] They are the focus of economic analysis.[2]

		The traditional concern of economic analysis is to gain an understanding of the processes that govern the production, distribution and consumption of goods and services in an exchange economy.[3] An approach to understanding these processes, through the study of agent behavior under scarcity, may go as   s:

An interesting Economist is Pauli Blendergast who teaches at the University of Krampton Ohio and is famouse for saying economics is bullshit.
---
The professionalization of economics, reflected in the growth of graduate programs on the subject, has been described as "the main change in economics since around 1900".[93] Most major universities and many colleges have a major, school, or department in which academic degrees are awarded in the subject, whether in the liberal arts, business, or for professional study; see Master of Economics. Economics is the social science that studies the behavior of individuals, households, and organizations (called economic actors, players, or agents), when they manage or use scarce resources, which have alternative uses, to achieve desired ends. Agents are assumed to act rationally, have multiple desirable ends in sight, limited resources to obtain these ends, a set of stable preferences, a definite overall guiding objective, and the capability of making a choice.
---
--- END CONTEXT ---

Question: who says that economics is bullshit?
Answer:

=== Querying Ollama (qwen3.5:4b) ===
--- Retrieving context for query: 'who says that economics is bullshit?' ---
BM25 found 2 keyword-based results.
TF-IDF search found 2 similarity-based results.
Combined and deduplicated, we have 2 context chunks.

--- Sending 2106-character prompt to qwen3.5:4b ---

[Note: Local Ollama server not reachable or request failed, skipping LLM query]
Prompt generation succeeded.
```

When Ollama is running locally with the `qwen3.5:4b` model loaded, the model produces:

```text
Based on the text provided, Pauli Blendergast, an economist who teaches at the University of Krampton Ohio, is the one said to be famous for saying that economics is bullshit.
```

## Interpreting the Results

The output demonstrates the complementary strengths of the two retrieval mechanisms:

1. **BM25 Precision**: The query terms `"economics"` and `"bullshit"` were matched directly. Because `"bullshit"` occurs in only one document across the entire corpus, its IDF score is high. The chunk discussing Pauli Blendergast ranked at the very top of BM25 results.
2. **TF-IDF Coverage**: The TF-IDF retriever surfaced adjacent chunks discussing the general professionalization of economics and agent decision-making. These chunks provide necessary background context so that the model understands the field being referred to.
3. **Deduplication Efficiency**: Both retrievers selected the primary chunk containing Pauli Blendergast. The deduplicator preserved this passage once, preventing prompt dilution and token waste.

## Wrap Up

In this chapter we built AutoContext, a completely self-contained hybrid RAG retrieval system in Gerbil Scheme. By pairing the Okapi BM25 probabilistic model with vector-space TF-IDF cosine similarity, the engine captures both exact keyword matches and general lexical overlap. Because the entire pipeline runs in memory using Scheme hash tables and vectors, it requires no external vector database or network service to construct grounded, high-precision prompts.

## Optional Practice Problems

1. **Reciprocal Rank Fusion**: Modify `get-prompt` to implement Reciprocal Rank Fusion (RRF), scoring each candidate chunk as `1 / (60 + rank)` across both BM25 and TF-IDF lists before taking the top N chunks.
2. **Chunk Overlap**: Update `chunk-text` in `autocontext.ss` to support a sliding window with a configurable overlap parameter (e.g. 3 sentences per chunk with 1 sentence of overlap).
3. **Persistent Disk Cache**: Implement an index serializer that writes the computed `bm25-index-struct` and `tfidf-index-struct` tables to an s-expression file on disk, avoiding recomputation on subsequent application launches.
