;; File: tfidf.ss
;; A simple vector-space retriever: TF-IDF weighted sparse vectors compared
;; with cosine similarity.
;;
;; The Common Lisp AutoContext uses neural sentence embeddings (via a Python
;; helper) as its "dense" retriever. To keep this example pure Gerbil with no
;; external services, we use TF-IDF vectors instead. They play the same role
;; -- a second, similarity-based ranking that complements BM25 -- and the API
;; (vector per chunk, cosine similarity, top-n indices) is the same, so a
;; neural embedder can be dropped in later.

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
