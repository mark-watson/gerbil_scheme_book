;;; RDF.ss ── simple in-memory RDF data store with a toy SPARQL subset
;;;
;;; Supports:
;;;   make-store        – create an empty triple store
;;;   add-triple        – add an (s p o) triple
;;;   remove-triple     – remove a matching (s p o) triple
;;;   store-triples     – retrieve all triples from a store
;;;   sparql-select     – run a SELECT query (variables + WHERE patterns)
;;;   print-all-triples – display all triples for debugging
;;;
;;; SPARQL subset handled:
;;;   SELECT ?var1 ?var2 ... WHERE { s p o . s p o . }
;;;   SELECT * WHERE { ... }
;;;   Variables begin with '?'

(export make-store add-triple remove-triple store-triples
        sparql-select print-all-triples)

;;;; ──────────────────────────────────────────────────────────────────
;;;;  1.  Data model
;;;; ──────────────────────────────────────────────────────────────────

;; A store is just a cons-cell box wrapping a list of triples.
;; Each triple is a 3-element list: (subject predicate object).

(define (make-store) (cons 'rdf-store '()))

(define (store-triples st) (cdr st))

(define (add-triple st s p o)
  "Add the triple (s p o) to store ST; returns ST for chaining."
  (set-cdr! st (cons (list s p o) (cdr st)))
  st)

(define (remove-triple st s p o)
  "Remove the first triple matching (s p o) from store ST."
  (set-cdr! st
            (filter (lambda (t)
                      (not (and (equal? (car t)   s)
                                (equal? (cadr t)  p)
                                (equal? (caddr t) o))))
                    (cdr st)))
  st)

(define (print-all-triples st)
  "Display every triple in the store."
  (display "All triples:\n")
  (for-each (lambda (t)
              (display "  ")
              (display (car t))   (display " ")
              (display (cadr t))  (display " ")
              (display (caddr t)) (newline))
            (store-triples st))
  (newline))

;;;; ──────────────────────────────────────────────────────────────────
;;;;  2.  Tokenizer
;;;; ──────────────────────────────────────────────────────────────────

(define (char-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch #\return)))

(define (tokenize str)
  "Split STR into a list of tokens; {}, .,; each become their own token."
  (let ((tokens '())
        (buf    '()))

    (define (emit!)
      (when (pair? buf)
        (set! tokens (cons (list->string (reverse buf)) tokens))
        (set! buf '())))

    (let loop ((chars (string->list str)))
      (if (null? chars)
          (begin
            (emit!)
            (reverse tokens))
          (let ((ch (car chars)))
            (cond
             ((member ch '(#\( #\) #\{ #\} #\. #\, #\;))
              (emit!)
              (set! tokens (cons (string ch) tokens)))
             ((char-whitespace? ch)
              (emit!))
             (else
              (set! buf (cons ch buf))))
            (loop (cdr chars)))))))

;;;; ──────────────────────────────────────────────────────────────────
;;;;  3.  SPARQL parser
;;;; ──────────────────────────────────────────────────────────────────

(define (var? tok)
  "True if TOK is a SPARQL variable (starts with '?')."
  (and (string? tok)
       (> (string-length tok) 0)
       (char=? (string-ref tok 0) #\?)))

(define (string-ci=? a b)
  (string=? (string-downcase a) (string-downcase b)))

(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;; Drop leading tokens until test passes, return remaining list.
(define (drop-until pred lst)
  (cond
   ((null? lst)         '())
   ((pred (car lst))    lst)
   (else                (drop-until pred (cdr lst)))))

(define (parse-sparql query)
  "Parse a minimal SELECT … WHERE { … } query.
   Returns two values: (vars patterns).
   vars     – list of variable name strings (or '(\"*\") for SELECT *)
   patterns – list of (s p o) patterns"

  (let* ((toks    (map string-downcase (tokenize query)))
         ;; advance to SELECT keyword
         (after-select
          (let ((rest (drop-until (lambda (t) (string=? t "select")) toks)))
            (if (null? rest) '() (cdr rest)))))

    ;; collect SELECT variables (or * wildcard)
    (let-values (((vars after-vars)
                  (if (and (pair? after-select)
                           (string=? (car after-select) "*"))
                      (values '("*") (cdr after-select))
                      (let loop ((ts after-select) (acc '()))
                        (if (or (null? ts) (not (var? (car ts))))
                            (values (reverse acc) ts)
                            (loop (cdr ts) (cons (car ts) acc)))))))

      ;; skip to '{'
      (let* ((after-brace
              (let ((rest (drop-until (lambda (t) (string=? t "{")) after-vars)))
                (if (null? rest) '() (cdr rest)))))

        ;; read triple patterns until '}'
        (let loop ((ts after-brace) (patterns '()))
          (cond
           ((null? ts)
            (values vars (reverse patterns)))
           ((string=? (car ts) "}")
            (values vars (reverse patterns)))
           (else
            ;; consume s p o
            (let* ((s  (car ts))
                   (ts (cdr ts))
                   (p  (if (pair? ts) (car ts) ""))
                   (ts (if (pair? ts) (cdr ts) '()))
                   (o  (if (pair? ts) (car ts) ""))
                   (ts (if (pair? ts) (cdr ts) '()))
                   ;; optional trailing '.'
                   (ts (if (and (pair? ts) (string=? (car ts) "."))
                           (cdr ts) ts)))
              (loop ts (cons (list s p o) patterns))))))))))

;;;; ──────────────────────────────────────────────────────────────────
;;;;  4.  Query evaluation
;;;; ──────────────────────────────────────────────────────────────────

;; An environment (binding set) is an association list: ((var . val) ...)

(define (env-lookup var env)
  "Return the value bound to VAR in ENV, or #f."
  (let ((pair (assoc var env)))
    (and pair (cdr pair))))

(define (env-extend var val env)
  "Return a new env with VAR=VAL added, or #f on conflict."
  (let ((existing (assoc var env)))
    (cond
     (existing
      (if (equal? (cdr existing) val) env #f))
     (else
      (cons (cons var val) env)))))

(define (match-pattern pattern triple env)
  "Try to unify one PATTERN with one TRIPLE given ENV.
   Returns extended env on success, #f on failure."
  (let ((ps (car pattern))   (pp (cadr pattern))   (po (caddr pattern))
        (ts (car triple))    (tp (cadr triple))     (to (caddr triple)))
    (let* ((e1 (if (var? ps)
                   (env-extend ps ts env)
                   (and (equal? ps ts) env)))
           (e2 (and e1
                    (if (var? pp)
                        (env-extend pp tp e1)
                        (and (equal? pp tp) e1))))
           (e3 (and e2
                    (if (var? po)
                        (env-extend po to e2)
                        (and (equal? po to) e2)))))
      e3)))

(define (join-patterns patterns triples envs)
  "For each pattern in PATTERNS, extend every env in ENVS by matching
   against all TRIPLES.  Returns the list of surviving envs."
  (if (null? patterns)
      envs
      (let* ((pat  (car patterns))
             (rest (cdr patterns))
             (new-envs
              (apply append
                     (map (lambda (env)
                            (filter
                             (lambda (e) e)  ;; remove #f
                             (map (lambda (triple)
                                    (match-pattern pat triple env))
                                  triples)))
                          envs))))
        (join-patterns rest triples new-envs))))

(define (project-env vars env)
  "Pick out only the selected VARS from ENV.
   Returns an alist ((var . val) ...)."
  (map (lambda (v) (cons v (env-lookup v env))) vars))

(define (sparql-select st query)
  "Run a SELECT query against store ST.
   Returns a list of alists, each mapping variable names to values.
   With SELECT *, returns the full binding environment as an alist."
  (let-values (((vars patterns) (parse-sparql query)))
    (let* ((triples   (store-triples st))
           (solutions (join-patterns patterns triples (list '())))
           (select-*? (and (= (length vars) 1)
                          (string=? (car vars) "*"))))
      (map (lambda (env)
             (if select-*?
                 env                          ;; full binding set
                 (project-env vars env)))
           solutions))))

;;;; ──────────────────────────────────────────────────────────────────
;;;;  5.  Display helpers
;;;; ──────────────────────────────────────────────────────────────────

(define (display-results results)
  "Pretty-print a list of query result alists."
  (if (null? results)
      (display "  (no results)\n")
      (for-each (lambda (row)
                  (display "  ")
                  (for-each (lambda (pair)
                              (display (car pair))
                              (display ": ")
                              (display (cdr pair))
                              (display "  "))
                            row)
                  (newline))
                results)))

(define (run-query st label q)
  (display "Query: ") (display label) (newline)
  (display-results (sparql-select st q))
  (newline))

;;;; ──────────────────────────────────────────────────────────────────
;;;;  6.  Demo
;;;; ──────────────────────────────────────────────────────────────────

(let ((st (make-store)))

  ;; Load some data
  (add-triple st "<ex:alice>" "<foaf:name>"  "\"Alice\"")
  (add-triple st "<ex:alice>" "<foaf:knows>" "<ex:bob>")
  (add-triple st "<ex:alice>" "<foaf:age>"   "30")
  (add-triple st "<ex:bob>"   "<foaf:name>"  "\"Bob\"")
  (add-triple st "<ex:bob>"   "<foaf:age>"   "25")
  (add-triple st "<ex:carol>" "<foaf:name>"  "\"Carol\"")
  (add-triple st "<ex:carol>" "<foaf:age>"   "35")

  (print-all-triples st)

  ;; Who does Alice know, and what is their name?
  (run-query st
    "Who does Alice know?"
    "SELECT ?friendName WHERE {
       <ex:alice> <foaf:knows> ?friend .
       ?friend    <foaf:name>  ?friendName .
     }")

  ;; All subjects and their names
  (run-query st
    "All names"
    "SELECT ?s ?name WHERE {
       ?s <foaf:name> ?name .
     }")

  ;; All subjects and their ages
  (run-query st
    "All ages"
    "SELECT ?person ?age WHERE {
       ?person <foaf:age> ?age .
     }")

  ;; SELECT * – return every binding
  (run-query st
    "SELECT * (all triples)"
    "SELECT * WHERE { ?s ?p ?o . }")

  ;; Remove a triple and verify
  (display "After removing <ex:bob> <foaf:age> 25:\n")
  (remove-triple st "<ex:bob>" "<foaf:age>" "25")
  (run-query st
    "All ages after removal"
    "SELECT ?person ?age WHERE {
       ?person <foaf:age> ?age .
     }"))
