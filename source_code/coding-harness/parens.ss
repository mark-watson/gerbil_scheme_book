;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; parens.ss -- a delimiter-balance scanner for Gerbil Scheme source.
;;;
;;; gxc reports reader failures with no position at all ("Incomplete form,
;;; EOF reached", "Datum or EOF expected"), which is the worst possible
;;; message for the most common Lisp mistake. This module scans source text
;;; the way the Gerbil reader does -- strings, |symbols|, ; line comments,
;;; nested #| block |# comments, #\x character literals -- and reports every
;;; unbalanced (), [] or {} with a line and column, plus a hint about where
;;; the missing or extra delimiter most likely belongs.
;;;
;;; It also records the extent of every top-level form, which the harness
;;; reuses for outline_file and for splitting gxi_eval input into forms.

(import :std/sugar
        :std/format
        :std/srfi/13)

(export scan-parens
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

(defstruct paren-report (problems hints forms) transparent: #t)

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
;; scan-parens : string -> paren-report

(def (scan-parens text)
  (let ((n (string-length text))
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
      (when (and (pair? stack) (= col 1) (not form-hinted?))
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

    (let lp ((i 0) (line 1) (col 1))
      (if (>= i n)
        (void)
        (let ((c (string-ref text i)))
          (cond
           ((char=? c #\newline) (lp (+ i 1) (+ line 1) 1))
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

    (make-paren-report (reverse problems) (reverse hints) (reverse forms))))

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
    (format "~a: delimiters balanced (~a top-level forms)" label (length (paren-report-forms report)))
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
