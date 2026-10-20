;; File: microgpt.ss
;; Andrej Karpathy's microGPT -- a tiny, dependency-free GPT -- in Gerbil Scheme.
;; Ported from the Common Lisp version in "Loving Common Lisp".
;;
;; Everything is built from scratch on scalar values:
;;   - a reverse-mode autograd engine (the `value` struct, like micrograd)
;;   - a 1-layer transformer: token + position embeddings, RMSNorm,
;;     multi-head causal self-attention with a KV cache, and a ReLU MLP
;;   - an Adam optimiser with linear learning-rate decay
;;   - temperature-scaled sampling to "hallucinate" new names
;;
;; Usage:
;;   gxi microgpt.ss            ; interpreted (slow), 1000 steps
;;   make exe && ./microgpt 300 ; compiled (fast), 300 training steps

(import :std/format :std/sort :std/misc/ports)
(export main run-microgpt)

;;; ------------------------------------------------------------------------
;;; Utilities
;;; ------------------------------------------------------------------------

(def pi (* 4.0 (atan 1.0)))

;; Box-Muller sample from N(mean, std).
(def (random-gauss mean std)
  (+ mean (* std
             (sqrt (* -2.0 (log (max (random-real) 1e-15))))
             (cos (* 2.0 pi (random-real))))))

;; Fisher-Yates shuffle, returning a new list.
(def (shuffle-list lst)
  (let ((v (list->vector lst)))
    (do ((i (- (vector-length v) 1) (- i 1)))
        ((< i 1) (vector->list v))
      (let* ((j (random-integer (+ i 1)))
             (tmp (vector-ref v i)))
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)))))

(def (string-trim-whitespace s)
  (let* ((chars (string->list s))
         (ws? (lambda (c) (char-whitespace? c)))
         (drop-ws (lambda (l) (let loop ((l l)) (if (and (pair? l) (ws? (car l))) (loop (cdr l)) l)))))
    (list->string (reverse (drop-ws (reverse (drop-ws chars)))))))

;; Non-empty, trimmed lines of FILE (or '() if it does not exist).
(def (read-lines file)
  (if (file-exists? file)
    (filter (lambda (s) (> (string-length s) 0))
            (map string-trim-whitespace (read-file-lines file)))
    '()))

;; Elements [start, end) of list L.
(def (sublist l start end)
  (let loop ((l (list-tail l start)) (n (- end start)) (acc '()))
    (if (= n 0) (reverse acc) (loop (cdr l) (- n 1) (cons (car l) acc)))))

(def (index-of ch str)
  (let loop ((i 0))
    (cond ((= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

;;; ------------------------------------------------------------------------
;;; Hyper-parameters and global model state
;;; ------------------------------------------------------------------------

(def n-layer 1)
(def n-embd 16)
(def block-size 16)
(def n-head 4)
(def head-dim 4)

(def docs '())          ; training strings
(def uchars "")         ; sorted unique characters (the vocabulary)
(def bos 0)             ; beginning/end-of-sequence token id
(def vocab-size 0)
(def state-dict (make-hash-table))   ; name -> matrix (vector of vectors of value)
(def params '())                     ; every trainable value, in a fixed order

;;; ------------------------------------------------------------------------
;;; Autograd: a scalar `value` that remembers how it was computed
;;; ------------------------------------------------------------------------

(defstruct value (data grad children local-grads))

(def (mkv data (children '()) (local-grads '()))
  (make-value (exact->inexact data) 0.0 children local-grads))

;; Lift plain numbers into constant values.
(def (ensure v) (if (value? v) v (mkv v)))

(def (v+ a b)
  (let ((a (ensure a)) (b (ensure b)))
    (mkv (+ (value-data a) (value-data b)) (list a b) '(1.0 1.0))))

(def (v* a b)
  (let ((a (ensure a)) (b (ensure b)))
    (mkv (* (value-data a) (value-data b)) (list a b)
         (list (value-data b) (value-data a)))))

(def (vpow v p)
  (let ((v (ensure v)))
    (mkv (expt (value-data v) p) (list v)
         (list (* p (expt (value-data v) (- p 1)))))))

(def (vlog v)
  (let ((v (ensure v)))
    (mkv (log (value-data v)) (list v) (list (/ 1.0 (value-data v))))))

(def (vexp v)
  (let* ((v (ensure v)) (e (exp (value-data v))))
    (mkv e (list v) (list e))))

(def (vrelu v)
  (let ((v (ensure v)))
    (mkv (max 0.0 (value-data v)) (list v)
         (list (if (> (value-data v) 0.0) 1.0 0.0)))))

(def (vneg v) (v* v -1.0))
(def (v- a b) (v+ a (vneg b)))
(def (v/ a b) (v* a (vpow b -1.0)))

;; Reverse-mode backpropagation: topologically sort the graph that produced
;; V, then push gradients from V back to every node with the chain rule.
(def (backward v)
  (let ((topo '())
        (seen (make-hash-table-eq)))
    (let build ((n v))
      (unless (hash-get seen n)
        (hash-put! seen n #t)
        (for-each build (value-children n))
        (set! topo (cons n topo))))
    (value-grad-set! v 1.0)
    (for-each (lambda (n)
                (for-each (lambda (c g)
                            (value-grad-set! c (+ (value-grad c) (* g (value-grad n)))))
                          (value-children n) (value-local-grads n)))
              topo)))

;;; ------------------------------------------------------------------------
;;; Model parameters
;;; ------------------------------------------------------------------------

;; NOUT x NIN matrix (vector of row vectors) of small random values.
(def (make-matrix nout nin (std 0.08))
  (let ((m (make-vector nout)))
    (do ((i 0 (+ i 1))) ((= i nout) m)
      (let ((row (make-vector nin)))
        (do ((j 0 (+ j 1))) ((= j nin))
          (let ((v (mkv (random-gauss 0.0 std))))
            (set! params (cons v params))
            (vector-set! row j v)))
        (vector-set! m i row)))))

(def (init-model)
  (set! state-dict (make-hash-table))
  (set! params '())
  (let ((mat (lambda (k r c) (hash-put! state-dict k (make-matrix r c)))))
    (mat "wte" vocab-size n-embd)
    (mat "wpe" block-size n-embd)
    (mat "lm_head" vocab-size n-embd)
    (do ((i 0 (+ i 1))) ((= i n-layer))
      (let ((lmat (lambda (s r c) (mat (format "layer~a.~a" i s) r c))))
        (lmat "attn_wq" n-embd n-embd)
        (lmat "attn_wk" n-embd n-embd)
        (lmat "attn_wv" n-embd n-embd)
        (lmat "attn_wo" n-embd n-embd)
        (lmat "mlp_fc1" (* 4 n-embd) n-embd)
        (lmat "mlp_fc2" n-embd (* 4 n-embd)))))
  (set! params (reverse params)))

;;; ------------------------------------------------------------------------
;;; Architecture
;;; ------------------------------------------------------------------------

(def (vsum vs) (foldl (lambda (v acc) (v+ acc v)) (mkv 0.0) vs))

;; Matrix-vector product: W (vector of rows) times X (list of values).
(def (vlinear x w)
  (map (lambda (row) (vsum (map v* (vector->list row) x)))
       (vector->list w)))

(def (vsoftmax logits)
  (let* ((mx (apply max (map value-data logits)))
         (exps (map (lambda (v) (vexp (v- v mx))) logits))
         (total (vsum exps)))
    (map (lambda (e) (v/ e total)) exps)))

(def (vrmsnorm x)
  (let ((scale (vpow (v+ (v/ (vsum (map (lambda (xi) (v* xi xi)) x)) (length x))
                         1e-5)
                     -0.5)))
    (map (lambda (xi) (v* xi scale)) x)))

;; One KV-cache slot per layer; each slot holds a list of key (or value)
;; vectors, one per position processed so far.
(def (make-kv-cache) (make-vector n-layer '()))

(def (cache-push! cache li item)
  (vector-set! cache li (append (vector-ref cache li) (list item))))

;; Forward pass for a single token at position POS-ID. Returns logits.
(def (gpt tok-id pos-id keys vals)
  (let* ((sd (lambda (li s) (hash-ref state-dict (format "layer~a.~a" li s))))
         (x (vrmsnorm (map v+
                           (vector->list (vector-ref (hash-ref state-dict "wte") tok-id))
                           (vector->list (vector-ref (hash-ref state-dict "wpe") pos-id))))))
    (do ((li 0 (+ li 1))) ((= li n-layer))
      ;; 1) multi-head causal self-attention with residual connection
      (let* ((xr x)
             (xn (vrmsnorm x))
             (q (vlinear xn (sd li "attn_wq")))
             (k (vlinear xn (sd li "attn_wk")))
             (v (vlinear xn (sd li "attn_wv"))))
        (cache-push! keys li k)
        (cache-push! vals li v)
        (let* ((heads
                (apply append
                       (map (lambda (h)
                              (let* ((hs (* h head-dim))
                                     (he (+ hs head-dim))
                                     (q-h (sublist q hs he))
                                     (k-h (map (lambda (ki) (sublist ki hs he)) (vector-ref keys li)))
                                     (v-h (map (lambda (vi) (sublist vi hs he)) (vector-ref vals li)))
                                     ;; attention weights over all cached positions
                                     (aw (vsoftmax
                                          (map (lambda (kt)
                                                 (v/ (vsum (map v* q-h kt)) (sqrt head-dim)))
                                               k-h))))
                                ;; weighted sum of value vectors
                                (let loop ((j 0) (acc '()))
                                  (if (= j head-dim)
                                    (reverse acc)
                                    (loop (+ j 1)
                                          (cons (vsum (map (lambda (w vt) (v* w (list-ref vt j)))
                                                           aw v-h))
                                                acc))))))
                            (iota n-head))))
               (attn-out (vlinear heads (sd li "attn_wo"))))
          (set! x (map v+ attn-out xr))))
      ;; 2) MLP with residual connection
      (let* ((xr x)
             (h (map vrelu (vlinear (vrmsnorm x) (sd li "mlp_fc1"))))
             (out (vlinear h (sd li "mlp_fc2"))))
        (set! x (map v+ out xr))))
    (vlinear x (hash-ref state-dict "lm_head"))))

;;; ------------------------------------------------------------------------
;;; Training
;;; ------------------------------------------------------------------------

(def (tokenize doc)
  (append (list bos)
          (map (lambda (ch) (index-of ch uchars)) (string->list doc))
          (list bos)))

(def (run-training steps)
  (let* ((lr 0.01) (b1 0.85) (b2 0.99) (eps 1e-8)
         (pvec (list->vector params))
         (np (vector-length pvec))
         (m (make-vector np 0.0))
         (mv (make-vector np 0.0))
         (ndocs (length docs)))
    (do ((step 0 (+ step 1))) ((= step steps))
      (let* ((tokens (list->vector (tokenize (list-ref docs (modulo step ndocs)))))
             (n (min block-size (- (vector-length tokens) 1)))
             (keys (make-kv-cache))
             (vals (make-kv-cache))
             ;; cross-entropy loss: -log p(next token), averaged over positions
             (losses (let loop ((pos 0) (acc '()))
                       (if (= pos n)
                         acc
                         (let* ((logits (gpt (vector-ref tokens pos) pos keys vals))
                                (probs (list->vector (vsoftmax logits))))
                           (loop (+ pos 1)
                                 (cons (vneg (vlog (vector-ref probs (vector-ref tokens (+ pos 1)))))
                                       acc))))))
             (loss (v/ (vsum losses) n))
             (t1 (+ step 1))
             (lr-t (* lr (- 1.0 (/ step (exact->inexact steps))))))
        (backward loss)
        ;; Adam update with bias correction and linear LR decay
        (do ((i 0 (+ i 1))) ((= i np))
          (let* ((p (vector-ref pvec i))
                 (g (value-grad p)))
            (vector-set! m i (+ (* b1 (vector-ref m i)) (* (- 1 b1) g)))
            (vector-set! mv i (+ (* b2 (vector-ref mv i)) (* (- 1 b2) g g)))
            (let ((mh (/ (vector-ref m i) (- 1 (expt b1 t1))))
                  (vh (/ (vector-ref mv i) (- 1 (expt b2 t1)))))
              (value-data-set! p (- (value-data p) (/ (* lr-t mh) (+ (sqrt vh) eps))))
              (value-grad-set! p 0.0))))
        (when (or (< t1 10) (zero? (modulo t1 25)) (= t1 steps))
          (printf "step ~a/~a | loss ~a\n" t1 steps
                  (/ (round (* 10000 (value-data loss))) 10000.0)))))))

;;; ------------------------------------------------------------------------
;;; Inference
;;; ------------------------------------------------------------------------

;; Sample an index from the (unnormalised) weight vector.
(def (random-choice weights)
  (let* ((n (vector-length weights))
         (r (* (random-real) (foldl + 0.0 (vector->list weights)))))
    (let loop ((i 0) (acc 0.0))
      (if (= i (- n 1))
        i
        (let ((acc (+ acc (vector-ref weights i))))
          (if (< r acc) i (loop (+ i 1) acc)))))))

(def (run-inference (num-samples 20) (temperature 0.5))
  (displayln "\n--- inference (new, hallucinated names) ---")
  (do ((i 0 (+ i 1))) ((= i num-samples))
    (let ((keys (make-kv-cache)) (vals (make-kv-cache)))
      (let loop ((pos 0) (tok bos) (sample '()))
        (if (= pos block-size)
          (printf "sample ~a: ~a\n" (+ i 1) (list->string (reverse sample)))
          (let* ((lt (map (lambda (l) (/ (value-data l) temperature))
                          (gpt tok pos keys vals)))
                 (mx (apply max lt))
                 (probs (list->vector (map (lambda (v) (exp (- v mx))) lt)))
                 (next (random-choice probs)))
            (if (= next bos)
              (printf "sample ~a: ~a\n" (+ i 1) (list->string (reverse sample)))
              (loop (+ pos 1) next (cons (string-ref uchars next) sample)))))))))

;;; ------------------------------------------------------------------------
;;; Main
;;; ------------------------------------------------------------------------

(def (run-microgpt steps: (steps 1000) names-file: (names-file "names.txt"))
  (random-source-randomize! default-random-source)
  (set! docs (shuffle-list (read-lines names-file)))
  (when (null? docs)
    (error "No training data: missing or empty file" names-file))
  (printf "num docs: ~a\n" (length docs))
  (let ((chars (make-hash-table)))
    (for-each (lambda (doc) (for-each (lambda (c) (hash-put! chars c #t)) (string->list doc)))
              docs)
    (set! uchars (list->string (sort (hash-keys chars) char<?)))
    (set! bos (string-length uchars))
    (set! vocab-size (+ bos 1)))
  (printf "vocab size: ~a\n" vocab-size)
  (init-model)
  (printf "num params: ~a\n" (length params))
  (run-training steps)
  (run-inference))

;; Command line: [steps] [names-file]
(def (main . args)
  (run-microgpt steps: (if (pair? args) (string->number (car args)) 1000)
                names-file: (if (and (pair? args) (pair? (cdr args))) (cadr args) "names.txt")))
