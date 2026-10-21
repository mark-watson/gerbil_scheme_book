;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; checker.ss -- the `make check` gate that runs after every accepted edit.
;;;
;;; Ordinary failures (a failing test, a type error) go back to the model.
;;; Syntax errors -- above all mismatched parens -- do NOT: models are bad at
;;; counting parens, and a model "fixing" its own paren error tends to move
;;; the problem around while rewriting the whole file. Instead the harness
;;; pinpoints the error (line, column, excerpt, likely fix) and asks the user
;;; to repair it in their editor, then re-runs the check:
;;;
;;;   [Enter] re-run the check   r revert the edit   m let the model try
;;;   a abort, keep the file as it is
;;;
;;; With no Makefile in the project, a built-in check runs instead: the paren
;;; scanner plus `gxc -S` (macro-expand only, no C compile) on the file.

(import :std/sugar
        :std/format
        :std/srfi/13
        :std/pregexp
        "config"
        "util"
        "parens"
        "approval")

(export run-check
        check-with-user-fix
        syntax-trouble
        paren-check-file
        MAX-CHECK-OUTPUT-CHARS)

(def MAX-CHECK-OUTPUT-CHARS 2500)

;; Gambit/Gerbil reader messages. gxc prints these with no line number.
(def READER-ERROR-PATTERNS
  '("Incomplete form" "Datum or EOF expected" "error reading module"
    "Datum expected" "Invalid token" "Unbalanced"))

;; ---------------------------------------------------------------------------
;; Running the check

(def (has-makefile?)
  (or (file-exists? "Makefile") (file-exists? "makefile") (file-exists? "GNUmakefile")))

(def (check-args) (split-words (gerbil-setting 'check_command)))

;; run-check : (listof path) -> (values output exit-code label)
(def (run-check paths)
  (let ((args (check-args)))
    (if (and (pair? args) (string=? (car args) "make") (not (has-makefile?)))
      (builtin-check paths)
      (let-values (((out code) (run-command args)))
        (if (and (not (= code 0)) (string-contains out "No rule to make target"))
          (builtin-check paths)
          (values out code (format "`~a`" (string-join args " "))))))))

(def (builtin-check paths)
  (let ((label "built-in check (paren scan + gxc -S; no Makefile/check target)")
        (sources (filter (lambda (p) (and (scheme-source-path? p) (file-exists? p))) paths))
        (outdir (temp-file-name "gxh-check")))
    (let loop ((ps sources) (acc []) (code 0))
      (if (null? ps)
        (begin
          (run-command ["rm" "-rf" outdir])
          (values (string-join (reverse acc) "\n") code label))
        (let* ((p (car ps))
               (text (read-file p))
               (report (scan-parens text)))
          (if (not (parens-ok? report))
            (loop (cdr ps) (cons (format-paren-report p text report) acc) 1)
            (let-values (((out c) (run-command ["gxc" "-S" "-d" outdir p])))
              (loop (cdr ps)
                    (cons (if (= c 0) (format "~a: gxc -S ok" p) out) acc)
                    (if (= c 0) code c)))))))))

;; ---------------------------------------------------------------------------
;; Finding the syntax error

(def (paren-check-file path)
  (let* ((text (read-file path))
         (report (scan-parens text)))
    (values (parens-ok? report) (format-paren-report path text report))))

;; Source files the check output mentions, e.g. ... form: "/abs/foo.ss"
(def (mentioned-sources output)
  (let loop ((start 0) (acc []))
    (let ((m (pregexp-match-positions "\"([^\"\n]+\\.(?:ss|scm|sld))\"" output start)))
      (if m
        (let ((path (substring output (car (cadr m)) (cdr (cadr m)))))
          (loop (cdr (car m)) (if (member path acc) acc (cons path acc))))
        (reverse acc)))))

;; Expander errors carry positions: "foo.ss"@12.3-12.9
(def (expander-locations output)
  (let loop ((start 0) (acc []))
    (let ((m (pregexp-match-positions "\"([^\"\n]+)\"@([0-9]+)\\.([0-9]+)" output start)))
      (if m
        (let ((loc (list (substring output (car (cadr m)) (cdr (cadr m)))
                         (string->number (substring output (car (caddr m)) (cdr (caddr m))))
                         (string->number (substring output (car (cadddr m)) (cdr (cadddr m)))))))
          (loop (cdr (car m)) (if (member loc acc) acc (cons loc acc))))
        (reverse acc)))))

;; syntax-trouble : output (listof path) -> #f | (kind . diagnostics)
;; kind is 'reader (unbalanced delimiters / unreadable file) or 'syntax
;; (the file reads but the Gerbil expander rejects a form).
(def (syntax-trouble output edited-paths)
  (let* ((candidates (filter (lambda (p) (and (scheme-source-path? p) (file-exists? p)))
                             (append edited-paths (mentioned-sources output))))
         (paren-reports
          (filter (lambda (x) x)
                  (map (lambda (p)
                         (let-values (((ok? text) (paren-check-file p)))
                           (and (not ok?) text)))
                       (unique candidates)))))
    (cond
     ((pair? paren-reports)
      (cons 'reader (string-join paren-reports "\n")))
     ((ormap (lambda (pat) (string-contains output pat)) READER-ERROR-PATTERNS)
      (cons 'reader
            (string-append
             "The Gerbil reader could not read a file, but every delimiter balances. "
             "Look for a stray '#', a bad character literal (#\\x...), or an unterminated #| comment.")))
     ((string-contains output "Syntax Error")
      (cons 'syntax
            (let ((locs (filter (lambda (l) (file-exists? (car l))) (expander-locations output))))
              (if (null? locs)
                "Gerbil expander reported a Syntax Error (see the output above)."
                (string-join
                 (map (lambda (l)
                        (format "~a, line ~a, col ~a:\n~a" (car l) (cadr l) (caddr l)
                                (source-excerpt (read-file (car l)) (cadr l) (caddr l))))
                      locs)
                 "\n")))))
     (else #f))))

;; Drop paths naming a file already seen ("m.ss" and "/abs/dir/m.ss").
(def (unique lst)
  (let loop ((l lst) (seen []) (acc []))
    (cond ((null? l) (reverse acc))
          ((member (path-expand (car l)) seen) (loop (cdr l) seen acc))
          (else (loop (cdr l) (cons (path-expand (car l)) seen) (cons (car l) acc))))))

(def (user-should-fix? kind)
  (or (eq? kind 'reader)
      (string=? (gerbil-setting 'ask_user_on) "syntax")))

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
                         (format "~a FAILED with a SYNTAX ERROR (exit ~a). The user asks you to fix it. Re-read the file, then propose a corrected version; count delimiters carefully and call check_parens on your result.\n\nDiagnostics:\n~a\n\nCheck output:\n~a"
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
  (displayln (colorize BOLD "Please fix this syntax error in your editor (the harness does not let the model guess at paren fixes)."))
  (displayln "  [Enter]  I fixed it: re-run the check")
  (when revert
    (displayln "  r        revert this edit"))
  (displayln "  m        hand the diagnostics to the model and let it try")
  (displayln "  a        abort: leave the file as it is and stop this task"))
