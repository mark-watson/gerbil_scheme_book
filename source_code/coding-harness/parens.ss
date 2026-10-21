;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; parens.ss -- a delimiter-balance scanner for source code.
;;;
;;; gxc reports reader failures with no position at all ("Incomplete form,
;;; EOF reached", "Datum or EOF expected"), which is the worst possible
;;; message for the most common Lisp mistake. This module scans source text
;;; the way the language's reader does and reports every unbalanced (), []
;;; or {} with a line and column, plus a hint about where the missing or
;;; extra delimiter most likely belongs.
;;;
;;; The lexicon says which characters hide delimiters:
;;;
;;;   lisp     Gerbil/Scheme: strings, |symbols|, ; line comments, nested
;;;            #| block |# comments, #\x characters, #<<here-strings
;;;   python   # comments; '...' "..." and triple-quoted strings
;;;   haskell  -- and nested {- -} comments; "..." and 'c' (but not x')
;;;   c-like   // and /* */ comments; '...' "..." `...`; /regex/ literals
;;;            (JavaScript, TypeScript, and most curly-brace languages)
;;;
;;; For Lisp it also records the extent of every top-level form, which the
;;; harness reuses for outline_file and for splitting Gerbil eval input.

(import :std/sugar
        :std/format
        :std/srfi/13)

(export scan-parens
        scan-delimiters
        lexicons
        parens-ok?
        paren-report-problems
        paren-report-hints
        paren-report-forms
        paren-problem-kind
        paren-problem-line
        paren-problem-col
        paren-problem-message
        paren-form-line
        paren-form-end-line
        paren-form-start
        paren-form-end
        paren-form-head
        format-paren-report
        source-excerpt
        top-level-form-strings
        scheme-source-path?)

;; kind    : 'unexpected-close | 'mismatch | 'unclosed | 'unterminated-string
;;           | 'unterminated-symbol | 'unterminated-comment
;; line/col: 1-based position the message is about
(defstruct paren-problem (kind line col message) transparent: #t)

;; start/end are character offsets (end is exclusive), so a form's text is
;; (substring text start end). head is the first line of the form, trimmed.
(defstruct paren-form (line end-line start end head) transparent: #t)

(defstruct paren-report (problems hints forms lexicon) transparent: #t)

(def MAX-PROBLEMS 10)

(def (parens-ok? report)
  (null? (paren-report-problems report)))

(def (scheme-source-path? path)
  (member (path-extension path) '(".ss" ".scm" ".sld" ".ssi" ".ssxi" ".sls")))

(def (closer-for c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) (else #f)))

(def (opener? c) (memv c '(#\( #\[ #\{)))
(def (closer? c) (memv c '(#\) #\] #\})))

;; The first line of the form beginning at offset i, trimmed to 60 chars.
(def (head-at text i)
  (let* ((n (string-length text))
         (nl (let lp ((j i)) (if (or (>= j n) (char=? (string-ref text j) #\newline)) j (lp (+ j 1)))))
         (s (string-trim-both (substring text i nl))))
    (if (> (string-length s) 60) (string-append (substring s 0 57) "...") s)))

;; ---------------------------------------------------------------------------
;; Lexicons for the non-Lisp languages

;; strings: list of (delimiter multi-line?), longest delimiter first
(defstruct lexer (line-comments block-comment nested-block? strings char-literals? regex-literals?))

(def LEXERS
  (hash (python  (make-lexer ["#"] #f #f [["\"\"\"" #t] ["'''" #t] ["\"" #f] ["'" #f]] #f #f))
        (haskell (make-lexer ["--"] ["{-" "-}"] #t [["\"" #f]] #t #f))
        (c-like  (make-lexer ["//"] ["/*" "*/"] #f [["`" #t] ["\"" #f] ["'" #f]] #f #t))))

(def (lexicons) (cons 'lisp (hash-keys LEXERS)))

;; Haskell: "--" starts a comment only when the run of dashes is not part
;; of an operator such as --> or |--.
(def HASKELL-SYMBOL-CHARS (string->list "!#$%&*+./<=>?@\\^|~:"))

;; JavaScript: after one of these (or at the start) a '/' begins a regex
;; literal; after an identifier, number or closer it is division.
(def REGEX-PRECEDERS (string->list "(,=:[!&|?{};+-*%<>~^"))
(def REGEX-KEYWORDS '("return" "typeof" "case" "in" "of" "yield" "await" "void" "delete" "throw" "new" "else" "do"))

(def (ident-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\_ #\' #\$))))

;; ---------------------------------------------------------------------------
;; scan-delimiters : string symbol -> paren-report

(def (scan-parens text) (scan-delimiters text 'lisp))

(def (scan-delimiters text lexicon)
  (let ((n (string-length text))
        (lisp? (eq? lexicon 'lisp))
        (lx (hash-get LEXERS lexicon))
        (prev #f)           ; last significant char (regex-literal detection)
        (stack [])          ; open delimiters, innermost first: #(char line col offset)
        (problems [])
        (hints [])
        (forms [])
        (form-hinted? #f))  ; already hinted about the current top-level form?

    (def (problem! kind line col msg)
      (when (< (length problems) MAX-PROBLEMS)
        (set! problems (cons (make-paren-problem kind line col msg) problems))))

    (def (hint! msg) (set! hints (cons msg hints)))

    (def (outer) (and (pair? stack) (last-pair* stack)))

    (def (last-pair* lst) (if (null? (cdr lst)) (car lst) (last-pair* (cdr lst))))

    (def (describe-open o)
      (format "'~a' opened at line ~a, col ~a" (vector-ref o 0) (vector-ref o 1) (vector-ref o 2)))

    ;; Skip a "..." string or |symbol| body starting just after the opening
    ;; delimiter. Returns (values next-i line col) or #f values at EOF.
    (def (skip-quoted i line col delim)
      (let lp ((i i) (line line) (col col))
        (cond
         ((>= i n) (values #f line col))
         (else
          (let ((c (string-ref text i)))
            (cond
             ((char=? c #\\)
              (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline))
                (lp (+ i 2) (+ line 1) 1)
                (lp (+ i 2) line (+ col 2))))
             ((char=? c delim) (values (+ i 1) line (+ col 1)))
             ((char=? c #\newline) (lp (+ i 1) (+ line 1) 1))
             (else (lp (+ i 1) line (+ col 1)))))))))

    ;; Skip a (possibly nested) #| ... |# comment starting after the "#|".
    (def (skip-block-comment i line col)
      (let lp ((i i) (line line) (col col) (depth 1))
        (cond
         ((>= i n) (values #f line col))
         ((and (< (+ i 1) n) (char=? (string-ref text i) #\|) (char=? (string-ref text (+ i 1)) #\#))
          (if (= depth 1)
            (values (+ i 2) line (+ col 2))
            (lp (+ i 2) line (+ col 2) (- depth 1))))
         ((and (< (+ i 1) n) (char=? (string-ref text i) #\#) (char=? (string-ref text (+ i 1)) #\|))
          (lp (+ i 2) line (+ col 2) (+ depth 1)))
         ((char=? (string-ref text i) #\newline) (lp (+ i 1) (+ line 1) 1 depth))
         (else (lp (+ i 1) line (+ col 1) depth)))))

    (def (newline-at-or-after i)
      (let lp ((j i)) (if (or (>= j n) (char=? (string-ref text j) #\newline)) j (lp (+ j 1)))))

    ;; Skip a Gambit here-string #<<TAG ... TAG starting after the "#<<".
    ;; Returns (values offset-of-terminator-end line tag-length) or #f.
    (def (skip-heredoc i line)
      (let ((nl (newline-at-or-after i)))
        (if (>= nl n)
          (values #f line 0)
          (let ((tag (substring text i nl)))
            (let lp ((start (+ nl 1)) (line (+ line 1)))
              (let ((e (newline-at-or-after start)))
                (cond
                 ((string=? (substring text start e) tag) (values e line (string-length tag)))
                 ((>= e n) (values #f line 0))
                 (else (lp (+ e 1) (+ line 1))))))))))

    (def (open! c i line col)
      (when (null? stack)
        (set! form-hinted? #f))
      ;; A delimiter in column 1 while a top-level form is still open almost
      ;; always means the previous form is missing its closing paren(s).
      (when (and lisp? (pair? stack) (= col 1) (not form-hinted?))
        (let ((o (outer)))
          (set! form-hinted? #t)
          (hint! (format "line ~a starts a new form in column 1 (~a) while the form at line ~a (~a) is still open -- it is probably missing ~a closing delimiter(s) before line ~a"
                         line (head-at text i) (vector-ref o 1) (head-at text (vector-ref o 3))
                         (length stack) line))))
      (set! stack (cons (vector c line col i) stack)))

    (def (close! c i line col)
      (cond
       ((null? stack)
        (problem! 'unexpected-close line col
                  (format "unexpected '~a' at line ~a, col ~a: nothing is open here~a"
                          c line col
                          (if (pair? forms)
                            (format " (the previous top-level form, line ~a, was already complete at line ~a)"
                                    (paren-form-line (car forms)) (paren-form-end-line (car forms)))
                            ""))))
       (else
        (let* ((o (car stack))
               (want (closer-for (vector-ref o 0))))
          (unless (char=? c want)
            (problem! 'mismatch line col
                      (format "mismatched '~a' at line ~a, col ~a: expected '~a' to close ~a"
                              c line col want (describe-open o))))
          (set! stack (cdr stack))
          (when (null? stack)
            (set! forms (cons (make-paren-form (vector-ref o 1) line (vector-ref o 3) (+ i 1)
                                               (head-at text (vector-ref o 3)))
                              forms)))))))

    (def (starts-with-at? s i)
      (let ((m (string-length s)))
        (and (<= (+ i m) n) (string=? (substring text i (+ i m)) s))))

    ;; Non-Lisp languages: a line that starts in column 1 while a bracket is
    ;; still open usually means a closer is missing just above it.
    (def (column-1-hint! c i line)
      (when (and (pair? stack) (not form-hinted?) (not (char-whitespace? c)) (not (closer? c)))
        (let ((o (outer)))
          (set! form-hinted? #t)
          (hint! (format "line ~a starts in column 1 (~a) while ~a (~a) is still open -- it is probably missing ~a closing delimiter(s) before line ~a"
                         line (head-at text i) (describe-open o) (head-at text (vector-ref o 3))
                         (length stack) line)))))

    (def (line-comment-at i)
      (find (lambda (s)
              (and (starts-with-at? s i)
                   (or (not (eq? lexicon 'haskell))
                       (let dashes ((j i))
                         (cond ((>= j n) #t)
                               ((char=? (string-ref text j) #\-) (dashes (+ j 1)))
                               (else (not (memv (string-ref text j) HASKELL-SYMBOL-CHARS))))))))
            (lexer-line-comments lx)))

    ;; The offset just past a 'c' or '\n' char literal, or #f (x' is a name).
    (def (char-literal-end i)
      (and (or (= i 0) (not (ident-char? (string-ref text (- i 1)))))
           (< (+ i 2) n)
           (if (char=? (string-ref text (+ i 1)) #\\)
             (let lp ((j (+ i 3)))
               (cond ((or (>= j n) (> j (+ i 12))) #f)
                     ((char=? (string-ref text j) #\newline) #f)
                     ((char=? (string-ref text j) #\') (+ j 1))
                     (else (lp (+ j 1)))))
             (and (char=? (string-ref text (+ i 2)) #\')
                  (not (char=? (string-ref text (+ i 1)) #\newline))
                  (+ i 3)))))

    ;; The offset just past a /regex/flags literal, or #f.
    (def (regex-end i)
      (and (or (not prev) (memv prev REGEX-PRECEDERS) (member (word-before i) REGEX-KEYWORDS))
           (let lp ((j (+ i 1)) (in-class? #f))
             (if (>= j n)
               #f
               (let ((d (string-ref text j)))
                 (cond
                  ((char=? d #\newline) #f)
                  ((char=? d #\\) (lp (+ j 2) in-class?))
                  ((char=? d #\[) (lp (+ j 1) #t))
                  ((char=? d #\]) (lp (+ j 1) #f))
                  ((and (char=? d #\/) (not in-class?))
                   (let flags ((k (+ j 1)))
                     (if (and (< k n) (char-alphabetic? (string-ref text k))) (flags (+ k 1)) k)))
                  (else (lp (+ j 1) in-class?))))))))

    ;; The identifier just before offset i (skipping spaces), or "".
    (def (word-before i)
      (let* ((end (let lp ((j i)) (if (and (> j 0) (char-whitespace? (string-ref text (- j 1)))) (lp (- j 1)) j)))
             (start (let lp ((j end)) (if (and (> j 0) (ident-char? (string-ref text (- j 1)))) (lp (- j 1)) j))))
        (substring text start end)))

    ;; Skip a string body starting just after its opening quote q.
    ;; Returns (values next-i line col status), status 'ok | 'eol | 'eof.
    (def (skip-string i line col q multi?)
      (let ((m (string-length q)))
        (let lp ((i i) (line line) (col col))
          (cond
           ((>= i n) (values i line col 'eof))
           ((char=? (string-ref text i) #\\)
            (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline))
              (lp (+ i 2) (+ line 1) 1)
              (lp (+ i 2) line (+ col 2))))
           ((starts-with-at? q i) (values (+ i m) line (+ col m) 'ok))
           ((char=? (string-ref text i) #\newline)
            (if multi? (lp (+ i 1) (+ line 1) 1) (values i line col 'eol)))
           (else (lp (+ i 1) line (+ col 1)))))))

    ;; Skip a `template` body starting just after the backquote. Each ${...}
    ;; inside is code, which may hold strings and templates of its own.
    (def (skip-template i line col)
      (let lp ((i i) (line line) (col col))
        (cond
         ((>= i n) (values i line col 'eof))
         ((char=? (string-ref text i) #\\)
          (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline))
            (lp (+ i 2) (+ line 1) 1)
            (lp (+ i 2) line (+ col 2))))
         ((char=? (string-ref text i) #\`) (values (+ i 1) line (+ col 1) 'ok))
         ((starts-with-at? "${" i)
          (let-values (((j l2 c2 status) (skip-interpolation (+ i 2) line (+ col 2))))
            (if (eq? status 'ok) (lp j l2 c2) (values j l2 c2 'eof))))
         ((char=? (string-ref text i) #\newline) (lp (+ i 1) (+ line 1) 1))
         (else (lp (+ i 1) line (+ col 1))))))

    ;; Skip the code of a ${...} up to its closing brace.
    (def (skip-interpolation i line col)
      (set! prev #\()
      (let lp ((i i) (line line) (col col) (depth 0))
        (if (>= i n)
          (values i line col 'eof)
          (let ((c (string-ref text i)))
            (cond
             ((char=? c #\newline) (lp (+ i 1) (+ line 1) 1 depth))
             ((starts-with-at? "//" i)
              (let skip ((j i))
                (if (or (>= j n) (char=? (string-ref text j) #\newline))
                  (lp j line (+ col (- j i)) depth)
                  (skip (+ j 1)))))
             ((starts-with-at? "/*" i)
              (let-values (((j l2 c2) (skip-generic-block (+ i 2) line (+ col 2) "/*" "*/" #f)))
                (if j (lp j l2 c2 depth) (values n l2 c2 'eof))))
             ((memv c '(#\` #\" #\'))
              (let-values (((j l2 c2 status)
                            (if (char=? c #\`)
                              (skip-template (+ i 1) line (+ col 1))
                              (skip-string (+ i 1) line (+ col 1) (string c) #f))))
                (set! prev #\a)
                (if (eq? status 'ok) (lp j l2 c2 depth) (values j l2 c2 status))))
             ((and (char=? c #\/) (regex-end i))
              => (lambda (j) (set! prev #\a) (lp j line (+ col (- j i)) depth)))
             ((char=? c #\{) (set! prev c) (lp (+ i 1) line (+ col 1) (+ depth 1)))
             ((char=? c #\})
              (if (= depth 0)
                (values (+ i 1) line (+ col 1) 'ok)
                (begin (set! prev c) (lp (+ i 1) line (+ col 1) (- depth 1)))))
             (else
              (unless (char-whitespace? c) (set! prev c))
              (lp (+ i 1) line (+ col 1) depth)))))))

    ;; Skip a block comment starting just after its opener.
    (def (skip-generic-block i line col open close nested?)
      (let lp ((i i) (line line) (col col) (depth 1))
        (cond
         ((>= i n) (values #f line col))
         ((starts-with-at? close i)
          (let ((m (string-length close)))
            (if (= depth 1)
              (values (+ i m) line (+ col m))
              (lp (+ i m) line (+ col m) (- depth 1)))))
         ((and nested? (starts-with-at? open i))
          (lp (+ i (string-length open)) line (+ col (string-length open)) (+ depth 1)))
         ((char=? (string-ref text i) #\newline) (lp (+ i 1) (+ line 1) 1 depth))
         (else (lp (+ i 1) line (+ col 1) depth)))))

    ;; One token of a non-Lisp language; k continues the scan.
    (def (generic-token c i line col k)
      (when (and (= col 1) (not (line-comment-at i)))
        (column-1-hint! c i line))
      (let ((block (lexer-block-comment lx)))
        (cond
         ((line-comment-at i)
          (let skip ((j i))
            (if (or (>= j n) (char=? (string-ref text j) #\newline))
              (k j line (+ col (- j i)))
              (skip (+ j 1)))))
         ((and block (starts-with-at? (car block) i))
          (let* ((m (string-length (car block))))
            (let-values (((j l2 c2) (skip-generic-block (+ i m) line (+ col m) (car block) (cadr block)
                                                        (lexer-nested-block? lx))))
              (if j
                (k j l2 c2)
                (problem! 'unterminated-comment line col
                          (format "block comment ~a starting at line ~a, col ~a is never closed with ~a"
                                  (car block) line col (cadr block)))))))
         ((find (lambda (s) (starts-with-at? (car s) i)) (lexer-strings lx))
          => (lambda (s)
               (let ((q (car s)) (m (string-length (car s))))
                 (let-values (((j l2 c2 status)
                               (if (and (eq? lexicon 'c-like) (string=? q "`"))
                                 (let-values (((j l2 c2 status) (skip-template (+ i m) line (+ col m))))
                                   (values j l2 c2 (if (eq? status 'ok) 'ok 'eof)))
                                 (skip-string (+ i m) line (+ col m) q (cadr s)))))
                   (set! prev #\a)
                   (case status
                     ((ok) (k j l2 c2))
                     ((eol)
                      (problem! 'unterminated-string line col
                                (format "string ~a...~a starting at line ~a, col ~a is not closed before the end of the line"
                                        q q line col))
                      (k j l2 c2))
                     (else
                      (problem! 'unterminated-string line col
                                (format "string ~a...~a starting at line ~a, col ~a is never closed (everything after it was read as part of it)"
                                        q q line col))))))))
         ((and (lexer-char-literals? lx) (char=? c #\') (char-literal-end i))
          => (lambda (j) (set! prev #\a) (k j line (+ col (- j i)))))
         ((and (lexer-regex-literals? lx) (char=? c #\/) (regex-end i))
          => (lambda (j) (set! prev #\a) (k j line (+ col (- j i)))))
         ((opener? c)
          (open! c i line col)
          (set! prev c)
          (k (+ i 1) line (+ col 1)))
         ((closer? c)
          (close! c i line col)
          (set! prev c)
          (k (+ i 1) line (+ col 1)))
         (else
          (unless (char-whitespace? c) (set! prev c))
          (k (+ i 1) line (+ col 1))))))

    (let lp ((i 0) (line 1) (col 1))
      (if (>= i n)
        (void)
        (let ((c (string-ref text i)))
          (cond
           ((char=? c #\newline) (lp (+ i 1) (+ line 1) 1))
           ((not lisp?) (generic-token c i line col lp))
           ((char=? c #\;)
            (let skip ((j i))
              (if (or (>= j n) (char=? (string-ref text j) #\newline))
                (lp j line (+ col (- j i)))
                (skip (+ j 1)))))
           ((or (char=? c #\") (char=? c #\|))
            (let-values (((j l2 c2) (skip-quoted (+ i 1) line (+ col 1) c)))
              (if j
                (lp j l2 c2)
                (begin
                  (problem! (if (char=? c #\") 'unterminated-string 'unterminated-symbol) line col
                            (format "~a starting at line ~a, col ~a is never closed (everything after it was read as part of it)"
                                    (if (char=? c #\") "string" "|symbol|") line col))
                  ;; A missing quote earlier flips which quotes open strings,
                  ;; so the reported start can be far below the real typo.
                  (let ((odd (odd-quote-lines text line)))
                    (when (pair? odd)
                      (hint! (format "line(s) ~a have an odd number of '\"' characters -- a quote is probably missing or extra there"
                                     (string-join (map number->string odd) ", ")))))))))
           ((and (char=? c #\#) (< (+ i 1) n))
            (let ((d (string-ref text (+ i 1))))
              (cond
               ((char=? d #\|)
                (let-values (((j l2 c2) (skip-block-comment (+ i 2) line (+ col 2))))
                  (if j
                    (lp j l2 c2)
                    (problem! 'unterminated-comment line col
                              (format "block comment #| starting at line ~a, col ~a is never closed with |#" line col)))))
               ((char=? d #\\)
                ;; #\( #\) #\" #\; ... : the char after the backslash is
                ;; always literal; then skip a name like #\space or #\x41.
                (let skip ((j (+ i 3)))
                  (if (and (< j n) (<= (+ i 3) n) (char-alphabetic-or-digit? (string-ref text j)))
                    (skip (+ j 1))
                    (lp (min j n) line (+ col (- (min j n) i))))))
               ((char=? d #\()
                (open! #\( i line col)
                (lp (+ i 2) line (+ col 2)))
               ((and (char=? d #\<) (< (+ i 2) n) (char=? (string-ref text (+ i 2)) #\<))
                (let-values (((j l2 taglen) (skip-heredoc (+ i 3) line)))
                  (if j
                    (lp j l2 (+ taglen 1))
                    (problem! 'unterminated-string line col
                              (format "here-string #<< starting at line ~a, col ~a has no terminating tag line" line col)))))
               (else (lp (+ i 1) line (+ col 1))))))
           ((opener? c)
            (open! c i line col)
            (lp (+ i 1) line (+ col 1)))
           ((closer? c)
            (close! c i line col)
            (lp (+ i 1) line (+ col 1)))
           (else (lp (+ i 1) line (+ col 1)))))))

    ;; Whatever is still open at EOF.
    (when (pair? stack)
      (let* ((o (outer))
             (missing (list->string (map (lambda (x) (closer-for (vector-ref x 0))) stack))))
        (problem! 'unclosed (vector-ref o 1) (vector-ref o 2)
                  (format "~a (~a) is never closed: EOF reached with ~a delimiter(s) still open; the minimal fix appends \"~a\" -- but check the hint for where it really belongs"
                          (describe-open o) (head-at text (vector-ref o 3)) (length stack) missing))
        (when (> (length stack) 1)
          (let ((inner (car stack)))
            (hint! (format "innermost unclosed delimiter: ~a" (describe-open inner)))))))

    (make-paren-report (reverse problems) (reverse hints) (reverse forms) lexicon)))

(def (char-alphabetic-or-digit? c)
  (or (char-alphabetic? c) (char-numeric? c)))

;; Lines up to and including `upto` that, read on their own, end inside a
;; string. Legit multi-line strings show up too, so at most 3 are returned.
(def (odd-quote-lines text upto)
  (let loop ((lines (string-split text #\newline)) (k 1) (acc []))
    (if (or (null? lines) (> k upto) (= (length acc) 3))
      (reverse acc)
      (loop (cdr lines) (+ k 1)
            (if (line-ends-in-string? (car lines)) (cons k acc) acc)))))

(def (line-ends-in-string? s)
  (let ((n (string-length s)))
    (let lp ((i 0) (in? #f))
      (cond
       ((>= i n) in?)
       ((and in? (char=? (string-ref s i) #\\)) (lp (+ i 2) in?))
       ((char=? (string-ref s i) #\") (lp (+ i 1) (not in?)))
       ((and (not in?) (char=? (string-ref s i) #\;)) #f)
       ((and (not in?) (char=? (string-ref s i) #\#) (< (+ i 2) n) (char=? (string-ref s (+ i 1)) #\\))
        (lp (+ i 3) in?))
       (else (lp (+ i 1) in?))))))

;; ---------------------------------------------------------------------------
;; Reporting

;; Lines [line-2, line+2] with a caret under col.
(def (source-excerpt text line col (context 2))
  (let* ((lines (list->vector (string-split text #\newline)))
         (nlines (vector-length lines))
         (from (max 1 (- line context)))
         (to (min nlines (+ line context))))
    (call-with-output-string
     (lambda (out)
       (let lp ((k from))
         (when (<= k to)
           (fprintf out "  ~a| ~a\n" (string-pad (number->string k) 5) (vector-ref lines (- k 1)))
           (when (= k line)
             (fprintf out "  ~a  ~a^\n" (make-string 5 #\space) (make-string (max 0 (- col 1)) #\space)))
           (lp (+ k 1))))))))

(def (format-paren-report label text report)
  (if (parens-ok? report)
    (if (eq? (paren-report-lexicon report) 'lisp)
      (format "~a: delimiters balanced (~a top-level forms)" label (length (paren-report-forms report)))
      (format "~a: delimiters balanced" label))
    (call-with-output-string
     (lambda (out)
       (fprintf out "~a: UNBALANCED DELIMITERS\n" label)
       (for-each (lambda (p)
                   (fprintf out "\n* ~a\n~a" (paren-problem-message p)
                            (source-excerpt text (paren-problem-line p) (paren-problem-col p))))
                 (paren-report-problems report))
       (for-each (lambda (h) (fprintf out "\nhint: ~a\n" h))
                 (paren-report-hints report))))))

;; Split text into the source strings of its top-level forms. Bare atoms
;; between forms are ignored, so callers should only pass parenthesized code.
(def (top-level-form-strings text)
  (map (lambda (f) (substring text (paren-form-start f) (paren-form-end f)))
       (paren-report-forms (scan-parens text))))
