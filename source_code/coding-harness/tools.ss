;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; tools.ss -- the tools the model can call.
;;;
;;; Files:     read_file  list_dir  grep  propose_edit
;;; Code:      outline_file  find_definition  check_delimiters
;;; Running:   run_shell  eval_code
;;; Reference: search_examples (only when an examples_dir is configured)
;;;
;;; The language profile (lang.ss) decides how outline_file, find_definition
;;; and eval_code work and which commands run_shell accepts.
;;;
;;; propose_edit is the only tool that writes. It pre-scans proposed source
;;; for unbalanced delimiters, shows a colored diff, asks y/n/s, and on
;;; approval writes the file and runs the `make check` gate (checker.ss),
;;; which hands syntax errors to the user rather than the model.

(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/13
        :std/text/json
        "config"
        "util"
        "parens"
        "lang"
        "approval"
        "checker")

(export tool-schemas
        execute-tool-calls
        enabled-tools
        run-eval
        outline-file
        search-examples)

(def MAX-TOOL-OUTPUT 12000)

;; ---------------------------------------------------------------------------
;; Registry

;; name -> (description params handler available?); params: list of
;; (name type description). A description may be a thunk, so it can name
;; the current language; available? is a thunk that hides the tool.
(def registry (make-hash-table))
(def TOOL-ORDER [])

(def (define-tool name params description handler (available? (lambda () #t)))
  (hash-put! registry name (list description params handler available?))
  (set! TOOL-ORDER (append TOOL-ORDER [name])))

(def (enabled-tools)
  (filter (lambda (name) ((cadddr (hash-ref registry name)))) TOOL-ORDER))

(def (text-of x) (if (procedure? x) (x) x))

(def (tool-schemas)
  (map (lambda (name)
         (let* ((entry (hash-ref registry name))
                (props (make-hash-table)))
           (for-each (lambda (p) (hash-put! props (car p) (hash (type (cadr p)) (description (text-of (caddr p))))))
                     (cadr entry))
           (hash (type "function")
                 (function (hash (name name)
                                 (description (text-of (car entry)))
                                 (parameters (hash (type "object")
                                                   (properties props)
                                                   (required (map car (cadr entry))))))))))
       (enabled-tools)))

;; ---------------------------------------------------------------------------
;; Dispatch

(def (short-args s)
  (let ((flat (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s)))
    (if (> (string-length flat) 120) (string-append (substring flat 0 117) "...") flat)))

(def (call-tool name args)
  (let ((entry (and (member name (enabled-tools)) (hash-get registry name))))
    (if (not entry)
      (format "Error: unknown tool '~a'. Available tools: ~a" name (string-join (enabled-tools) ", "))
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

;; Build output and dependency trees that grep should never wander into.
(def SKIPPED-DIRS '(".gerbil" ".git" "node_modules" "dist-newstyle" ".stack-work" "__pycache__"
                    ".venv" "venv" ".mypy_cache" ".pytest_cache" ".ruff_cache"))

(def (exclude-dir-flags) (map (lambda (d) (string-append "--exclude-dir=" d)) SKIPPED-DIRS))

;; Editor droppings and dotfiles (.git, .env, build output) stay out.
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
          (sort (filter (lambda (e) (not (or (hidden-file? e) (member e SKIPPED-DIRS))))
                        (directory-files path))
                string<?))
     "\n")))

(def (tool-grep pattern path)
  (let-values (((out code) (run-command (append ["grep" "-rnE"] (exclude-dir-flags) ["-e" pattern path]))))
    (cond
     ((= code 1) "(no matches)")
     (else (truncate-string out MAX-TOOL-OUTPUT)))))

;; A command is allowed when its words start with one of the whitelisted
;; prefixes: "npm test" allows `npm test -- --watch=false` but not `npm install`.
(def (allowed-command? tokens)
  (find (lambda (entry)
          (let ((words (split-words entry)))
            (and (<= (length words) (length tokens))
                 (equal? words (take-first tokens (length words))))))
        (shell-commands)))

(def (take-first lst n) (if (or (= n 0) (null? lst)) [] (cons (car lst) (take-first (cdr lst) (- n 1)))))

(def (tool-run-shell command)
  (let ((tokens (split-words command)))
    (cond
     ((null? tokens) "empty command")
     ((not (allowed-command? tokens))
      (format "Command '~a' is not whitelisted. Allowed (as command prefixes): ~a.~a"
              command (string-join (shell-commands) ", ")
              (if (language-evaluator (current-lang)) " (Use eval_code to run code.)" "")))
     ((find (lambda (a) (and (not (string-prefix? "-" a)) (hidden-file? (path-strip-directory a))))
            (cdr tokens))
      => (lambda (bad) (format "refusing to run a command that references a hidden/internal file: ~a" bad)))
     (else
      (let-values (((out code) (run-command tokens)))
        (format "~a(exit ~a)" (truncate-string out MAX-TOOL-OUTPUT) code))))))

;; ---------------------------------------------------------------------------
;; Code tools

(def (tool-check-delimiters path)
  (if (not (file-exists? path))
    (format "Error: ~a does not exist" path)
    (let-values (((ok? text) (paren-check-file path)))
      text)))

;; Lisp: the top-level forms from the scanner. Other languages: the lines
;; matching the language's outline pattern (definitions, classes, ...).
(def (outline-file path)
  (cond
   ((not (file-exists? path)) (format "Error: ~a does not exist" path))
   ((eq? (lexicon-for-path path) 'lisp)
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
         (string-append "\n\n" (format-paren-report path text report))))))
   (else
    (let* ((lang (or (language-for-path path) (current-lang)))
           (pattern (language-outline-pattern lang)))
      (let-values (((out code) (run-command ["grep" "-nE" "-e" pattern path]))
                   ((ok? report) (paren-check-file path)))
        (string-append
         (if (= code 0)
           (string-join
            (map (lambda (l)
                   (let ((colon (string-index l #\:)))
                     (format "~a  ~a" (string-pad (substring l 0 colon) 5)
                             (string-trim-right (substring l (+ colon 1) (string-length l))))))
                 (filter (lambda (l) (not (string=? l ""))) (string-split out #\newline)))
            "\n")
           (format "(no ~a definitions found in ~a; use read_file)" (language-title lang) path))
         (if ok? "" (string-append "\n\n" report))))))))

;; Characters that are special in a POSIX extended regex but legal in
;; identifiers in some language (list->vector!, *foo*, $el, x').
(def (regex-quote s)
  (list->string
   (append-map (lambda (c) (if (memv c (string->list ".[]()*+?{}|^$\\")) [#\\ c] [c]))
               (string->list s))))

(def (include-flags lang)
  (map (lambda (e) (string-append "--include=*" e)) (language-extensions lang)))

(def (tool-find-definition name)
  (let* ((lang (current-lang))
         (pattern (string-replace-all (language-def-pattern lang) "~a" (regex-quote name))))
    (let-values (((out code) (run-command (append ["grep" "-rnE"] (include-flags lang) (exclude-dir-flags)
                                                  ["-e" pattern "."]))))
      (if (= code 1)
        (format "No definition of ~a found under ~a (it may come from a library; try grep~a)"
                name (current-directory)
                (if (examples-dir) " or search_examples" ""))
        (truncate-string out MAX-TOOL-OUTPUT)))))

(def (examples-dir)
  (let ((d (setting 'examples_dir)))
    (and (string? d) (file-directory? (path-expand d)) (path-expand d))))

(def (search-examples pattern)
  (let ((dir (examples-dir)))
    (if (not dir)
      (format "No examples directory configured for ~a (set languages.~a.examples_dir in the harness config)"
              (language-title (current-lang)) (language-name (current-lang)))
      (let-values (((out code) (run-command (append ["grep" "-rnE" "-m" "4"] (include-flags (current-lang))
                                                    (exclude-dir-flags) ["--exclude-dir=coding-harness"
                                                                         "-e" pattern dir]))))
        (if (= code 1)
          (format "(no matches for ~s in ~a)" pattern dir)
          (truncate-string out MAX-TOOL-OUTPUT))))))

;; Run a snippet in a fresh process in the working directory, killed after
;; eval_timeout seconds.
(def (run-eval code)
  (let ((lang (current-lang)))
    (if (and (language-strict? lang)
             (not (parens-ok? (scan-delimiters code (language-lexicon lang)))))
      (format "not evaluated: ~a"
              (format-paren-report "code" code (scan-delimiters code (language-lexicon lang))))
      (let* ((tmpdir (temp-directory "harness-eval"))
             (args (eval-command code tmpdir))
             (secs (setting 'eval_timeout)))
        (try
         (if (not args)
           (format "eval_code is not available for ~a" (language-title lang))
           (let-values (((out status timed-out?) (run-command/timeout args secs)))
             (string-append (truncate-string out MAX-TOOL-OUTPUT)
                            (if timed-out?
                              (format "\n(killed after ~a s timeout)" secs)
                              (format "(exit ~a)" status)))))
         (finally (run-command ["rm" "-rf" tmpdir])))))))

(def (tool-eval-code code)
  (displayln (colorize BOLD (format "\nThe model wants to run this ~a code:" (language-title (current-lang)))))
  (displayln (colorize CYAN code))
  (if (or (auto-approve?)
          (eq? 'yes (ask-choice "Run it? [y]es / [n]o: " '(("y" . yes) ("yes" . yes) ("n" . no) ("no" . no)) 'no)))
    (run-eval code)
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
      (let* ((lexicon (lexicon-for-path path))
             (strict? (let ((l (language-for-path path))) (and l (language-strict? l))))
             (report (and lexicon (scan-delimiters new lexicon)))
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
         ;; Only Lisp is refused outright: elsewhere the scanner can be fooled
         ;; (unusual string syntax), and the check after writing catches a
         ;; real imbalance anyway.
         ((and (auto-approve?) (not balanced?) strict?)
          (format "refused: auto-approve never writes unbalanced code. Fix the delimiters and propose again.\n~a" paren-text))
         ((auto-approve?)
          (unless (quiet?) (displayln (colorize YELLOW "[auto-approve: applying]")))
          (apply-and-check path exists? current new))
         (else
          (case (ask-choice
                 (format "\nApply this change? [y]es / [n]o / [s]kip and tell the model why~a: "
                         (if balanced? "" " / [p] send the delimiter errors back to the model"))
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
  "List files and subdirectories (with trailing /). Hidden files, build output and dependency directories are excluded."
  tool-list-dir)

(define-tool "grep"
  '(("pattern" "string" "Extended regex (grep -E).")
    ("path" "string" "Directory or file to search."))
  "Recursively grep for PATTERN with line numbers (skips .git, node_modules, virtualenvs and build output)."
  tool-grep)

(define-tool "find_definition"
  '(("name" "string" "Identifier to look up, e.g. \"parse_row\" or \"ParseError\"."))
  (lambda ()
    (format "Find where a ~a identifier (function, class, type, constant, ...) is defined in the project's ~a files."
            (language-title (current-lang))
            (let ((exts (language-extensions (current-lang))))
              (if (null? exts) "source" (string-join exts "/")))))
  tool-find-definition)

(define-tool "outline_file"
  '(("path" "string" "Source file."))
  "List the definitions in a source file (top-level forms for Lisp; functions, classes and types for other languages) with line numbers. Cheaper than read_file for a big file. Also reports unbalanced delimiters."
  outline-file)

(define-tool "check_delimiters"
  '(("path" "string" "Source file to check."))
  "Check that every (), [] and {} in a source file balances, skipping strings and comments the way the language's reader does. Reports line/column and the likely fix for each problem."
  tool-check-delimiters)

(define-tool "search_examples"
  '(("pattern" "string" "Extended regex to look for in the example files, e.g. \"http-post\" or \"defstruct\"."))
  (lambda ()
    (format "Search a directory of working ~a example code for usage patterns. Use it to check the exact name and signature of a library function before using it."
            (language-title (current-lang))))
  search-examples
  (lambda () (and (examples-dir) #t)))

(define-tool "eval_code"
  `(("code" "string" ,(lambda () (language-eval-help (current-lang)))))
  (lambda ()
    (format "Run ~a code in a fresh process in the working directory and return its output. The user approves each run. Use it to try out a function or check how a library API behaves."
            (language-title (current-lang))))
  tool-eval-code
  (lambda () (and (language-evaluator (current-lang)) #t)))

(define-tool "run_shell"
  `(("command" "string" ,(lambda () (string-append "A whitelisted command (no shell, no pipes); it must start with one of: "
                                                  (string-join (shell-commands) ", ")))))
  "Run a whitelisted command and return its combined output and exit code. Use it to build, test and lint (`make check`, test runners, type checkers)."
  tool-run-shell)

(define-tool "propose_edit"
  '(("path" "string" "Path of the file to edit or create.")
    ("old" "string" "For an existing file: its exact current contents. For a new file: the empty string.")
    ("new" "string" "The complete new contents of the file."))
  "Propose an edit or a new file. The user sees a colored diff (with a warning if the delimiters do not balance) and approves it; then the file is written and the project check runs. Syntax errors found by the check may be shown to the user to fix by hand."
  tool-propose-edit)
