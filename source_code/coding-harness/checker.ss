;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; checker.ss -- the `make check` gate that runs after every accepted edit.
;;;
;;; Ordinary failures (a failing test, a type error) go back to the model.
;;; Syntax errors -- above all mismatched delimiters -- do NOT: models are
;;; bad at counting parens, and a model "fixing" its own paren error tends to
;;; move the problem around while rewriting the whole file. Instead the
;;; harness pinpoints the error (line, column, excerpt, likely fix) and asks
;;; the user to repair it in their editor, then re-runs the check:
;;;
;;;   [Enter] re-run the check   r revert the edit   m let the model try
;;;   a abort, keep the file as it is
;;;
;;; With no Makefile in the project, a built-in check runs instead: the
;;; delimiter scanner plus each language's own parser or compiler on the
;;; edited files (gxc -S, Python's ast.parse, ghc -fno-code, tsc --noEmit,
;;; node --check), or a project-wide build (cabal build, tsc -p) when the
;;; project has one. What counts as a syntax error comes from the language
;;; profiles in lang.ss.

(import :std/sugar
        :std/format
        :std/srfi/13
        :std/pregexp
        "config"
        "util"
        "parens"
        "lang"
        "approval")

(export run-check
        check-with-user-fix
        syntax-trouble
        paren-check-file
        error-locations
        MAX-CHECK-OUTPUT-CHARS)

(def MAX-CHECK-OUTPUT-CHARS 2500)

;; ---------------------------------------------------------------------------
;; Running the check

(def (has-makefile?)
  (or (file-exists? "Makefile") (file-exists? "makefile") (file-exists? "GNUmakefile")))

(def (check-args) (split-words (setting 'check_command)))

;; run-check : (listof path) -> (values output exit-code label)
(def (run-check paths)
  (let ((args (check-args)))
    (if (and (pair? args) (string=? (car args) "make") (not (has-makefile?)))
      (builtin-check paths)
      (let-values (((out code) (run-command args)))
        (if (and (not (= code 0)) (string-contains out "No rule to make target"))
          (builtin-check paths)
          (values out code (format "`~a`" (string-join args " "))))))))

;; Scan every edited source file; then run each language's project build
;; once, or its per-file check on the files that scanned clean.
(def (builtin-check paths)
  (let ((sources (filter (lambda (p) (and (source-path? p) (file-exists? p))) paths))
        (outdir (temp-directory "harness-check"))
        (acc [])
        (code 0)
        (tools []))
    (def (note! text c tool)
      (set! acc (cons text acc))
      (unless (= c 0) (set! code c))
      (when (and tool (not (member tool tools))) (set! tools (cons tool tools))))
    (def (run! args ok-text)
      (let-values (((out c) (run-command args)))
        (note! (if (= c 0) ok-text (trim out)) c (path-strip-directory (car args)))))
    (let* ((clean (filter (lambda (p)
                            (let-values (((ok? text) (paren-check-file p)))
                              (unless ok? (note! text 1 #f))
                              ok?))
                          sources))
           (langs (unique-langs (cons (current-lang) (filter-map language-for-path clean)))))
      (for-each
       (lambda (lang)
         (let ((mine (filter (lambda (p) (eq? (language-for-path p) lang)) clean))
               (project ((language-project-check lang))))
           (if (and project (or (pair? mine) (eq? lang (current-lang))))
             (run! project (format "~a: ok" (string-join project " ")))
             (for-each (lambda (p)
                         (let ((args ((language-file-check lang) p outdir)))
                           (if args
                             (run! args (format "~a: ~a ok" p (path-strip-directory (car args))))
                             (note! (format "~a: delimiters balanced (no ~a parser available for a deeper check)"
                                            p (language-title lang))
                                    0 #f))))
                       mine))))
       langs))
    (run-command ["rm" "-rf" outdir])
    (values (string-join (reverse acc) "\n")
            code
            (format "built-in check (delimiter scan~a; no Makefile/check target)"
                    (if (null? tools) "" (string-append " + " (string-join (reverse tools) ", ")))))))

(def (unique-langs langs)
  (let loop ((l langs) (acc []))
    (cond ((null? l) (reverse acc))
          ((memq (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

;; ---------------------------------------------------------------------------
;; Finding the syntax error

;; (values ok? report-text); a file type with no scanner rules counts as ok.
(def (paren-check-file path)
  (let ((lexicon (lexicon-for-path path)))
    (if (not lexicon)
      (values #t (format "~a: not scanned (no delimiter rules for this file type)" path))
      (let* ((text (read-file path))
             (report (scan-delimiters text lexicon)))
        (values (parens-ok? report) (format-paren-report path text report))))))

;; Positions in compiler output, as (path line col), for files inside the
;; project (a traceback through the standard library is not the user's to
;; fix). Formats: Gerbil "foo.ss"@12.3, Python File "foo.py", line 12,
;; GHC/node/most compilers foo.hs:12:3 or foo.js:12, tsc foo.ts(12,3).
(def (location-regexes)
  (let ((exts (string-join (map (lambda (e) (substring e 1 (string-length e))) (source-extensions-all)) "|")))
    [["\"([^\"\n]+)\"@([0-9]+)\\.([0-9]+)" 1 2 3]
     ["File \"([^\"\n]+)\", line ([0-9]+)" 1 2 #f]
     [(format "([^\\s\"'():]+\\.(?:~a))\\(([0-9]+),([0-9]+)\\)" exts) 1 2 3]
     [(format "([^\\s\"'():]+\\.(?:~a)):([0-9]+)(?::([0-9]+))?" exts) 1 2 3]]))

(def (inside-project? path)
  (string-prefix? (path-expand (current-directory)) (path-expand path)))

(def (error-locations output)
  (let ((found []))
    (for-each
     (lambda (spec)
       (let loop ((start 0))
         (let ((m (pregexp-match-positions (car spec) output start)))
           (when m
             (let* ((group (lambda (k)
                             (let ((pos (and k (list-ref m k))))
                               (and pos (substring output (car pos) (cdr pos))))))
                    (path (group (cadr spec)))
                    (line (string->number (group (caddr spec))))
                    (col (let ((c (group (cadddr spec)))) (or (and c (string->number c)) 1)))
                    (loc [path line col]))
               (when (and (file-exists? path) (inside-project? path) (not (member loc found)))
                 (set! found (cons loc found))))
             (loop (cdr (car m)))))))
     (location-regexes))
    (reverse found)))

(def (format-locations locs)
  (string-join
   (map (lambda (l)
          (format "~a, line ~a, col ~a:\n~a" (car l) (cadr l) (caddr l)
                  (source-excerpt (read-file (car l)) (cadr l) (caddr l))))
        locs)
   "\n"))

(def (matches-any? patterns output)
  (ormap (lambda (pat) (pregexp-match pat output)) patterns))

;; syntax-trouble : output (listof path) -> #f | (kind . diagnostics)
;; kind is 'reader (unbalanced delimiters / unreadable file) or 'syntax
;; (the file reads but the parser or expander rejects it).
(def (syntax-trouble output edited-paths)
  (let* ((locs (error-locations output))
         (candidates (filter (lambda (p) (and (source-path? p) (file-exists? p)))
                             (append edited-paths (map car locs))))
         (paren-reports
          (filter (lambda (x) x)
                  (map (lambda (p)
                         (let-values (((ok? text) (paren-check-file p)))
                           (and (not ok?) text)))
                       (unique candidates))))
         (langs (unique-langs (cons (current-lang) (filter-map language-for-path candidates))))
         (reader-lang (find (lambda (l) (matches-any? (language-reader-patterns l) output)) langs))
         (syntax-lang (find (lambda (l) (matches-any? (language-syntax-patterns l) output)) langs)))
    (cond
     ((pair? paren-reports)
      (cons 'reader (string-join paren-reports "\n")))
     (reader-lang
      (cons 'reader
            (if (null? locs)
              (language-reader-advice reader-lang)
              (string-append (language-reader-advice reader-lang) "\n" (format-locations locs)))))
     (syntax-lang
      (cons 'syntax
            (if (null? locs)
              (format "~a reported a syntax error (see the output above)." (language-title syntax-lang))
              (format-locations locs))))
     (else #f))))

;; Drop paths naming a file already seen ("m.ss" and "/abs/dir/m.ss").
(def (unique lst)
  (let loop ((l lst) (seen []) (acc []))
    (cond ((null? l) (reverse acc))
          ((member (path-expand (car l)) seen) (loop (cdr l) seen acc))
          (else (loop (cdr l) (cons (path-expand (car l)) seen) (cons (car l) acc))))))

;; ask_user_on: "syntax" (every syntax error), "reader" (only unbalanced or
;; unreadable files), "never" (everything goes to the model).
(def (user-should-fix? kind)
  (let ((policy (setting 'ask_user_on)))
    (cond ((equal? policy "never") #f)
          ((equal? policy "syntax") #t)
          (else (eq? kind 'reader)))))

;; ---------------------------------------------------------------------------
;; The gate
;;
;; check-with-user-fix : (listof path) (or procedure #f) -> (values status message)
;;   status : 'passed | 'failed | 'user-fixed | 'reverted | 'handed-to-model | 'aborted
;;   message: text for the model (tool result)
;; `revert` (a thunk, or #f when there is nothing to undo) restores the
;; pre-edit contents of the file.

(def (check-with-user-fix paths revert)
  (let loop ((round 0))
    (let-values (((out code label) (run-check paths)))
      (if (= code 0)
        (if (= round 0)
          (values 'passed (format "~a passed" label))
          (values 'user-fixed
                  (format "~a passed after the USER fixed a syntax error by hand. The file(s) ~a changed on disk: read_file them again before any further edit."
                          label (string-join paths ", "))))
        (let ((trouble (syntax-trouble out paths))
              (shown (truncate-string out MAX-CHECK-OUTPUT-CHARS)))
          (if (not (and trouble (user-should-fix? (car trouble)) (interactive?)))
            (values 'failed
                    (format "~a FAILED (exit ~a):\n~a~a" label code shown
                            (if trouble (string-append "\n\nDiagnostics:\n" (cdr trouble)) "")))
            (begin
              (print-syntax-report label shown (car trouble) (cdr trouble) revert)
              (case (ask-choice (colorize BOLD "fix> ")
                                (append '(("" . retry) ("m" . model) ("a" . abort))
                                        (if revert '(("r" . revert)) []))
                                'model)
                ((retry) (loop (+ round 1)))
                ((revert)
                 (revert)
                 (displayln (colorize YELLOW "Edit reverted."))
                 (values 'reverted
                         (format "SYNTAX ERROR after the edit; the user REVERTED it, so ~a is back to its previous contents. Diagnostics were:\n~a"
                                 (string-join paths ", ") (cdr trouble))))
                ((model)
                 (values 'handed-to-model
                         (format "~a FAILED with a SYNTAX ERROR (exit ~a). The user asks you to fix it. Re-read the file, then propose a corrected version; count delimiters carefully and call check_delimiters on your result.\n\nDiagnostics:\n~a\n\nCheck output:\n~a"
                                 label code (cdr trouble) shown)))
                (else
                 (values 'aborted
                         (format "~a FAILED with a SYNTAX ERROR that the user chose to leave unfixed for now. Do not retry; summarize and stop.\n\nDiagnostics:\n~a"
                                 label (cdr trouble))))))))))))

(def (print-syntax-report label output kind diagnostics revert)
  (displayln "")
  (displayln (colorize (string-append BOLD RED)
                       (format "━━ SYNTAX ERROR: ~a failed ~a━━"
                               label (if (eq? kind 'reader) "(unbalanced/unreadable) " ""))))
  (displayln output)
  (displayln (colorize YELLOW diagnostics))
  (displayln "")
  (displayln (colorize BOLD "Please fix this syntax error in your editor (the harness does not let the model guess at delimiter fixes)."))
  (displayln "  [Enter]  I fixed it: re-run the check")
  (when revert
    (displayln "  r        revert this edit"))
  (displayln "  m        hand the diagnostics to the model and let it try")
  (displayln "  a        abort: leave the file as it is and stop this task"))
