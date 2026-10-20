;; File: autocontext.ss
;; AutoContext: build effective one-shot LLM prompts from a directory of text
;; files that is too large to paste into a single context window.
;;
;; Pipeline:
;;   1. Load *.txt files and split them into chunks of a few sentences.
;;   2. Index the chunks with two retrievers: BM25 (keyword) and TF-IDF cosine
;;      similarity (vector space).
;;   3. For a query, take the top hits from each retriever, merge and
;;      deduplicate them by chunk index, and format a prompt with that context.
;;   4. Optionally send the prompt to a local Ollama model.

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
;; (The CL original splits on whitespace only, so "bullshit?" in a query would
;; not match "bullshit" in the text; normalising fixes that.)
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
