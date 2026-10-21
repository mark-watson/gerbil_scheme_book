;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; tools.ss -- the tools the model can call.
;;;
;;; General:  read_file  list_dir  grep  run_shell  propose_edit
;;; Gerbil:   check_parens  outline_file  find_definition  search_examples
;;;           gxi_eval
;;;
;;; propose_edit is the only tool that writes. It pre-scans proposed Gerbil
;;; source for unbalanced delimiters, shows a colored diff, asks y/n/s, and
;;; on approval writes the file and runs the `make check` gate (checker.ss),
;;; which hands syntax errors to the user rather than the model.

(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/13
        :std/text/json
        "config"
        "util"
        "parens"
        "approval"
        "checker")

(export tool-schemas
        execute-tool-calls
        gxi-eval
        outline-file
        search-examples
        ENABLED-TOOLS)

(def SHELL-WHITELIST '("make" "ls" "pwd" "cat" "head" "tail" "wc" "diff" "gxc" "gxpkg" "gerbil"))
(def MAX-TOOL-OUTPUT 12000)

;; ---------------------------------------------------------------------------
;; Registry

;; name -> (description params handler); params: list of (name type description)
(def registry (make-hash-table))
(def ENABLED-TOOLS [])

(def (define-tool name params description handler)
  (hash-put! registry name (list description params handler))
  (set! ENABLED-TOOLS (append ENABLED-TOOLS [name])))

(def (tool-schemas)
  (map (lambda (name)
         (let* ((entry (hash-ref registry name))
                (props (make-hash-table)))
           (for-each (lambda (p) (hash-put! props (car p) (hash (type (cadr p)) (description (caddr p)))))
                     (cadr entry))
           (hash (type "function")
                 (function (hash (name name)
                                 (description (car entry))
                                 (parameters (hash (type "object")
                                                   (properties props)
                                                   (required (map car (cadr entry))))))))))
       ENABLED-TOOLS))

;; ---------------------------------------------------------------------------
;; Dispatch

(def (short-args s)
  (let ((flat (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s)))
    (if (> (string-length flat) 120) (string-append (substring flat 0 117) "...") flat)))

(def (call-tool name args)
  (let ((entry (hash-get registry name)))
    (if (not entry)
      (format "Error: unknown tool '~a'. Available tools: ~a" name (string-join ENABLED-TOOLS ", "))
      (let* ((params (cadr entry))
             (missing (filter (lambda (p) (not (string? (json-ref args (car p))))) params)))
        (if (pair? missing)
          (format "Error: tool '~a' is missing string argument(s): ~a. Expected a JSON object with: ~a"
                  name (string-join (map car missing) ", ") (string-join (map car params) ", "))
          (try
           (let ((result (apply (caddr entry) (map (lambda (p) (json-ref args (car p))) params))))
             (if (string? result) result (format "~a" result)))
           (catch (e)
             (format "Error: tool '~a' raised: ~a" name
                     (if (error-object? e) (error-object-message e) e)))))))))

;; tool-calls -> list of (call-id name result)
(def (execute-tool-calls tool-calls)
  (map (lambda (tc)
         (let* ((id (json-ref tc 'id ""))
                (f (json-ref tc 'function (make-hash-table)))
                (name (json-ref f 'name ""))
                (raw (json-ref f 'arguments "{}"))
                (raw-str (if (string? raw) raw (json-object->string raw)))
                (args (if (hash-table? raw)
                        raw
                        (try (string->json-object raw-str) (catch (e) 'bad-json)))))
           (unless (quiet?)
             (displayln (colorize CYAN (format "* ~a ~a" name (short-args raw-str)))))
           (list id name
                 (cond
                  ((string=? (trim name) "")
                   (format "Error: the tool call was truncated (no function name). Arguments received: ~a" (short-args raw-str)))
                  ((eq? args 'bad-json)
                   (format "Error: invalid JSON in arguments for '~a': ~a" name (short-args raw-str)))
                  ((not (hash-table? args))
                   (format "Error: arguments for '~a' must be a JSON object" name))
                  (else (call-tool name args))))))
       tool-calls))

;; ---------------------------------------------------------------------------
;; File tools

;; Editor droppings and dotfiles (.git, .env, .gerbil build output) stay out.
(def (hidden-file? name)
  (or (string-suffix? "~" name)
      (and (string-prefix? "#" name) (string-suffix? "#" name))
      (and (string-prefix? "." name) (not (member name '("." ".."))))))

(def (tool-read-file path)
  (cond
   ((hidden-file? (path-strip-directory path))
    (format "refusing to read hidden/internal file: ~a" path))
   ((not (file-exists? path)) (format "Error: ~a does not exist" path))
   ((file-directory? path) (format "Error: ~a is a directory; use list_dir" path))
   (else (read-file path))))

(def (tool-list-dir path)
  (if (not (file-directory? path))
    (format "Error: ~a is not a directory" path)
    (string-join
     (map (lambda (e) (if (file-directory? (path-expand e path)) (string-append e "/") e))
          (sort (filter (lambda (e) (not (hidden-file? e))) (directory-files path)) string<?))
     "\n")))

(def (tool-grep pattern path)
  (let-values (((out code) (run-command ["grep" "-rnE" "--exclude-dir=.gerbil" "--exclude-dir=.git"
                                         "-e" pattern path])))
    (cond
     ((= code 1) "(no matches)")
     (else (truncate-string out MAX-TOOL-OUTPUT)))))

(def (tool-run-shell command)
  (let ((tokens (split-words command)))
    (cond
     ((null? tokens) "empty command")
     ((not (member (car tokens) SHELL-WHITELIST))
      (format "Command '~a' is not whitelisted. Allowed: ~a. (Use gxi_eval to evaluate Gerbil code.)"
              (car tokens) (string-join SHELL-WHITELIST ", ")))
     ((find (lambda (a) (and (not (string-prefix? "-" a)) (hidden-file? (path-strip-directory a))))
            (cdr tokens))
      => (lambda (bad) (format "refusing to run a command that references a hidden/internal file: ~a" bad)))
     (else
      (let-values (((out code) (run-command tokens)))
        (format "~a(exit ~a)" (truncate-string out MAX-TOOL-OUTPUT) code))))))

;; ---------------------------------------------------------------------------
;; Gerbil tools

(def (tool-check-parens path)
  (if (not (file-exists? path))
    (format "Error: ~a does not exist" path)
    (let-values (((ok? text) (paren-check-file path)))
      text)))

(def (outline-file path)
  (if (not (file-exists? path))
    (format "Error: ~a does not exist" path)
    (let* ((text (read-file path))
           (report (scan-parens text))
           (forms (paren-report-forms report)))
      (string-append
       (string-join
        (map (lambda (f)
               (format "~a-~a  ~a" (string-pad (number->string (paren-form-line f)) 5)
                       (string-pad-right (number->string (paren-form-end-line f)) 5)
                       (paren-form-head f)))
             forms)
        "\n")
       (if (parens-ok? report)
         ""
         (string-append "\n\n" (format-paren-report path text report)))))))

;; Characters that are special in a POSIX extended regex but legal in
;; Scheme identifiers (list->vector!, string->symbol, *foo*, ...).
(def (regex-quote s)
  (list->string
   (append-map (lambda (c) (if (memv c (string->list ".[]()*+?{}|^$\\")) [#\\ c] [c]))
               (string->list s))))

(def (append-map f lst) (apply append (map f lst)))

(def DEFINING-FORMS
  "def|define|def\\*|defvalues|defconst|defalias|definline|defstruct|defclass|defmethod|defrule|defrules|defsyntax|defsyntax-call|define-syntax|defgeneric|deferror-class|define-values|defcall-actor|defmessage")

(def (tool-find-definition name)
  (let* ((pattern (format "\\((~a)[[:space:]]+[(]?~a([[:space:])]|$)" DEFINING-FORMS (regex-quote name))))
    (let-values (((out code) (run-command ["grep" "-rnE" "--include=*.ss" "--include=*.scm" "--include=*.ssi"
                                           "--include=*.sld" "--exclude-dir=.gerbil" "--exclude-dir=.git"
                                           "-e" pattern "."])))
      (if (= code 1)
        (format "No definition of ~a found under ~a (it may come from an imported :std module; try search_examples)"
                name (current-directory))
        (truncate-string out MAX-TOOL-OUTPUT)))))

(def (search-examples pattern)
  (let ((dir (path-expand (gerbil-setting 'examples_dir))))
    (if (not (file-directory? dir))
      (format "Examples directory ~a not found (set gerbil.examples_dir in the harness config)" dir)
      (let-values (((out code) (run-command ["grep" "-rnE" "-m" "4" "--include=*.ss" "--exclude-dir=.gerbil"
                                             "--exclude-dir=gerbil-coding-harness" "-e" pattern dir])))
        (if (= code 1)
          (format "(no matches for ~s in ~a)" pattern dir)
          (truncate-string out MAX-TOOL-OUTPUT))))))

;; Evaluate Gerbil code in a fresh `gxi` in the working directory, so
;; (import "foo") loads ./foo.ss. gxi -e takes one form per flag, so the
;; code is split into top-level forms; if the last form is an expression its
;; value is printed with `write`.
(def NON-VALUE-HEADS '("def" "define" "import" "export" "defstruct" "defclass" "defrule" "defrules"
                       "defsyntax" "defmethod" "defvalues" "displayln" "display" "write" "pretty-print"
                       "for-each" "begin" "set!" "hash-put!"))

(def (form-head form)
  (let* ((s (substring form 1 (string-length form)))
         (end (or (string-index s (lambda (c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\]))))) (string-length s))))
    (substring s 0 end)))

(def (gxi-eval code)
  (let* ((report (scan-parens code))
         (forms (top-level-form-strings code)))
    (cond
     ((not (parens-ok? report))
      (format "not evaluated: ~a" (format-paren-report "code" code report)))
     (else
      (let* ((forms (if (null? forms) [(trim code)] forms))
             (last-form (last forms))
             (printed (if (and (string-prefix? "(" last-form)
                               (not (member (form-head last-form) NON-VALUE-HEADS)))
                        (format "(let ((v ~a)) (unless (void? v) (write v) (newline)))" last-form)
                        last-form))
             (all (append (drop-last forms) [printed]))
             (args (cons "gxi" (append-map (lambda (f) ["-e" f]) all)))
             (secs (gerbil-setting 'eval_timeout)))
        (let-values (((out code timed-out?) (run-command/timeout args secs)))
          (string-append (truncate-string out MAX-TOOL-OUTPUT)
                         (if timed-out?
                           (format "\n(killed after ~a s timeout)" secs)
                           (format "(exit ~a)" code)))))))))

(def (drop-last lst) (if (null? (cdr lst)) [] (cons (car lst) (drop-last (cdr lst)))))

(def (tool-gxi-eval code)
  (displayln (colorize BOLD "\nThe model wants to evaluate this Gerbil code with gxi:"))
  (displayln (colorize CYAN code))
  (if (or (auto-approve?)
          (eq? 'yes (ask-choice "Run it? [y]es / [n]o: " '(("y" . yes) ("yes" . yes) ("n" . no) ("no" . no)) 'no)))
    (gxi-eval code)
    "user declined to run this code"))

;; ---------------------------------------------------------------------------
;; propose_edit

(def (tool-propose-edit path old new)
  (let* ((exists? (file-exists? path))
         (current (if exists? (read-file path) "")))
    (cond
     ((and exists? (not (string=? current old)))
      (format "stale base: the on-disk contents of ~a do not match the 'old' you provided. read_file it again and retry." path))
     ((and exists? (string=? current new))
      "no changes (proposed content matches current file)")
     ((and (not exists?) (string=? new ""))
      "refused: cannot create an empty file")
     (else
      (let* ((report (and (scheme-source-path? path) (scan-parens new)))
             (balanced? (or (not report) (parens-ok? report)))
             (paren-text (if balanced? "" (format-paren-report path new report))))
        (displayln "")
        (unless exists? (displayln (colorize BOLD (format "(new file: ~a)" path))))
        (print-colored-diff (unified-diff current new (string-append "a/" path) (string-append "b/" path)))
        (unless balanced?
          (displayln (colorize YELLOW (string-append "\nWARNING: the proposed contents do not balance.\n" paren-text))))
        (cond
         ((dry-run?)
          "dry-run: diff shown, file not written")
         ((and (auto-approve?) (not balanced?))
          (format "refused: auto-approve never writes unbalanced code. Fix the delimiters and propose again.\n~a" paren-text))
         ((auto-approve?)
          (unless (quiet?) (displayln (colorize YELLOW "[auto-approve: applying]")))
          (apply-and-check path exists? current new))
         (else
          (case (ask-choice
                 (format "\nApply this change? [y]es / [n]o / [s]kip and tell the model why~a: "
                         (if balanced? "" " / [p] send the paren errors back to the model"))
                 (append '(("y" . yes) ("yes" . yes) ("n" . no) ("no" . no) ("s" . skip) ("skip" . skip))
                         (if balanced? [] '(("p" . parens))))
                 'no-user)
            ((yes) (apply-and-check path exists? current new))
            ((no) "user rejected the change")
            ((skip) (format "user skipped: ~a" (ask-reason)))
            ((parens)
             (format "user sent this back: the proposed contents of ~a have UNBALANCED DELIMITERS. Fix exactly these and propose again:\n~a"
                     path paren-text))
            (else "not applied: no interactive user to approve the edit (run with -y to auto-approve)")))))))))

(def (apply-and-check path existed? old-contents new)
  (write-file path new)
  (let-values (((status message)
                (check-with-user-fix [path]
                                     (lambda ()
                                       (if existed? (write-file path old-contents) (delete-file path))))))
    (if (eq? status 'reverted)
      message
      (string-append (if (auto-approve?) "applied (auto-approved); " "applied; ") message))))

;; ---------------------------------------------------------------------------
;; Registration

(define-tool "read_file"
  '(("path" "string" "File path relative to the working directory."))
  "Read and return the full contents of a file. Hidden/internal files (dotfiles, foo~, #foo#) are refused."
  tool-read-file)

(define-tool "list_dir"
  '(("path" "string" "Directory path. Use \".\" for the working directory."))
  "List files and subdirectories (with trailing /). Hidden files and .gerbil build output are excluded."
  tool-list-dir)

(define-tool "grep"
  '(("pattern" "string" "Extended regex (grep -E).")
    ("path" "string" "Directory or file to search."))
  "Recursively grep for PATTERN with line numbers (skips .gerbil and .git)."
  tool-grep)

(define-tool "find_definition"
  '(("name" "string" "Identifier to look up, e.g. \"scan-parens\" or \"my-struct\"."))
  "Find where a Gerbil identifier is defined (def, define, defstruct, defclass, defrule, defsyntax, defmethod, ...) in the project's .ss/.scm/.ssi files."
  tool-find-definition)

(define-tool "outline_file"
  '(("path" "string" "Gerbil source file."))
  "List the top-level forms of a Gerbil source file with their start-end line numbers (cheaper than read_file for a big file). Also reports unbalanced delimiters."
  outline-file)

(define-tool "check_parens"
  '(("path" "string" "Gerbil source file to check."))
  "Check that every (), [] and {} in a Gerbil file balances, reading strings, |symbols|, ; and #| |# comments and #\\x characters the way the Gerbil reader does. Reports line/column and the likely fix for each problem."
  tool-check-parens)

(define-tool "search_examples"
  '(("pattern" "string" "Extended regex to look for in the example .ss files, e.g. \"http-post\" or \"defstruct\"."))
  "Search the Gerbil Scheme book examples (working, idiomatic Gerbil code for JSON, HTTP, LLM APIs, FFI, CLI tools, actors, ...) for usage patterns. Use it to check the exact name and signature of a :std library function before using it."
  search-examples)

(define-tool "gxi_eval"
  '(("code" "string" "One or more Gerbil forms. (import \"foo\") loads ./foo.ss. The last expression's value is printed."))
  "Evaluate Gerbil code in a fresh gxi process in the working directory and return its output. The user approves each evaluation. Use it to try out a function or check how a :std API behaves."
  tool-gxi-eval)

(define-tool "run_shell"
  `(("command" "string" ,(string-append "A whitelisted command (no shell, no pipes): " (string-join SHELL-WHITELIST ", "))))
  "Run a whitelisted command and return its combined output and exit code. Use `make check` or `gxc -S file.ss` to verify code."
  tool-run-shell)

(define-tool "propose_edit"
  '(("path" "string" "Path of the file to edit or create.")
    ("old" "string" "For an existing file: its exact current contents. For a new file: the empty string.")
    ("new" "string" "The complete new contents of the file."))
  "Propose an edit or a new file. The user sees a colored diff (with a warning if the delimiters do not balance) and approves it; then the file is written and `make check` runs. Syntax errors found by the check are shown to the user to fix by hand."
  tool-propose-edit)
