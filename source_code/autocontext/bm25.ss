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
