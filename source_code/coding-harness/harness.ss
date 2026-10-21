;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; harness.ss -- a coding agent for Gerbil Scheme, Python, Haskell,
;;; TypeScript, JavaScript (and, with less help, any other language): REPL,
;;; slash commands, one-shot CLI.
;;;
;;;   gxi harness.ss                       interactive REPL
;;;   gxi harness.ss "add a test for foo"  one-shot
;;;   gxi harness.ss --help
;;;
;;; Based on the general-purpose coding-agent harness from "Practical
;;; Artificial Intelligence Development With Racket". The project language
;;; (lang.ss) supplies the idioms in the system prompt, an optional skill,
;;; language-aware tools (check_delimiters, outline_file, find_definition,
;;; eval_code, search_examples), and the syntax-error rules of the edit gate,
;;; where syntax errors after `make check` go to the user, not the model.

(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/13
        :std/misc/ports
        "config"
        "util"
        "parens"
        "lang"
        "approval"
        "llm"
        "checker"
        "tools")

(export main)

(def VERSION "0.2.0")

;; Exit codes for scripted (one-shot) use
(def EXIT-OK 0)
(def EXIT-MODEL-ERROR 1)
(def EXIT-CHECK-FAILED 2)
(def EXIT-REJECTED 3)
(def EXIT-SYNTAX-ERROR 4)
(def EXIT-BAD-ARGS 5)

;; ---------------------------------------------------------------------------
;; System prompt

(def SYSTEM-PROMPT-TEMPLATE #<<END
You are a coding assistant working in the directory {cwd}. The project language is {language}; write {language} unless the user asks for something else.

Workflow:
- Explore before editing: list_dir, outline_file (cheap overview of a source file), read_file, find_definition, grep.
- To EDIT a file: read_file it, then call propose_edit with its exact current contents as `old` and the complete new contents as `new`.
- To CREATE a file: call propose_edit with `old` set to "" (do not read_file a file that does not exist yet).
- One file per propose_edit call; keep each change focused.
- After every accepted edit the harness runs `{check}`. If its result says the USER fixed a syntax error by hand, read_file the file again before editing it.
- If the user rejects an edit, or the check fails for a reason other than syntax, explain what went wrong and fix it or ask; never resubmit the same edit.
- run_shell only accepts commands starting with: {whitelist}.
- When you are done, reply with a short summary of what changed.

END
  )

(def COMPACT-SYSTEM-PROMPT
  "You are a context compactor for a coding assistant. Summarize the conversation into a compact brief that will replace it. Preserve: the user's goals and instructions, decisions made, files created or modified (with paths), important code, check results and outstanding tasks. Dense bullets, no preamble.")

;; ---------------------------------------------------------------------------
;; Skills: ~/.agents/skills/<name>/SKILL.md

(def (skills-dir) (path-expand "~/.agents/skills"))
(def (skill-file name) (path-expand (string-append name "/SKILL.md") (skills-dir)))

(def (list-skills)
  (if (file-directory? (skills-dir))
    (sort (filter (lambda (d) (file-exists? (skill-file d))) (directory-files (skills-dir))) string<?)
    []))

(def (skill-description name)
  (let loop ((lines (cdr (string-split (read-file (skill-file name)) #\newline))))
    (cond
     ((null? lines) #f)
     ((string=? (trim (car lines)) "---") #f)
     ((string-prefix? "description:" (car lines)) (trim (substring (car lines) 12 (string-length (car lines)))))
     (else (loop (cdr lines))))))

(def use-skill? (make-parameter #t))
(def loaded-skill (make-parameter #f))   ; (name . chars) when autoloaded

(def (skill-section)
  (let ((name (setting 'skill)))
    (if (and (use-skill?) (setting 'autoload_skill) (string? name) (file-exists? (skill-file name)))
      (let ((content (read-file (skill-file name))))
        (loaded-skill (cons name (string-length content)))
        (format "\n\nReference material -- the '~a' skill (authoritative ~a idioms and APIs):\n\n~a"
                name (language-title (current-lang)) content))
      (begin (loaded-skill #f) ""))))

;; ---------------------------------------------------------------------------
;; Conversation state

(def messages (make-parameter []))

(def (system-prompt)
  (let ((lang (current-lang)))
    (string-append
     (fold-left (lambda (text kv) (string-replace-all text (car kv) (cdr kv)))
                SYSTEM-PROMPT-TEMPLATE
                [(cons "{cwd}" (path-expand (current-directory)))
                 (cons "{language}" (if (eq? lang (find-language "generic"))
                                      "not fixed (follow the existing code)"
                                      (language-title lang)))
                 (cons "{check}" (setting 'check_command))
                 (cons "{whitelist}" (string-join (shell-commands) ", "))])
     (language-prompt lang)
     (skill-section))))

(def (fold-left f acc lst)
  (if (null? lst) acc (fold-left f (f acc (car lst)) (cdr lst))))

(def (reset-conversation)
  (messages [(hash (role "system") (content (system-prompt)))]))

;; After /lang: same conversation, new system prompt.
(def (refresh-system-prompt)
  (messages (cons (hash (role "system") (content (system-prompt))) (cdr (messages)))))

(def (add-message! msg) (messages (append (messages) [msg])))

(def (msg-role m) (json-ref m 'role "?"))

(def (msg-tool-names m)
  (let ((tcs (json-ref m 'tool_calls)))
    (if (pair? tcs)
      (map (lambda (tc) (json-ref (json-ref tc 'function) 'name "?")) tcs)
      [])))

(def (msg-size m)
  (+ (string-length (message-content m))
     (let ((tcs (json-ref m 'tool_calls)))
       (if (pair? tcs)
         (apply + (map (lambda (tc)
                         (let ((a (json-ref (json-ref tc 'function) 'arguments "")))
                           (if (string? a) (string-length a) 0)))
                       tcs))
         0))))

;; ---------------------------------------------------------------------------
;; /context, /history, /compact

(def (one-line s width)
  (let ((flat (string-join (split-words (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s)) " ")))
    (if (> (string-length flat) width) (string-append (substring flat 0 (- width 1)) "…") flat)))

(def (show-context)
  (let* ((msgs (messages))
         (total (apply + (map msg-size msgs))))
    (displayln (format "\nContext: ~a messages, ~a chars, ~~~a tokens (est.)\n" (length msgs) total (quotient total 4)))
    (let loop ((ms msgs) (i 1))
      (when (pair? ms)
        (let* ((m (car ms))
               (preview (cond
                         ((string=? (msg-role m) "tool")
                          (format "[~a] ~a" (json-ref m 'name "?") (message-content m)))
                         ((pair? (msg-tool-names m))
                          (format "[tool calls: ~a] ~a" (string-join (msg-tool-names m) ", ") (message-content m)))
                         (else (message-content m)))))
          (displayln (format " ~a  ~a ~a  ~a"
                             (string-pad (number->string i) 3)
                             (string-pad-right (msg-role m) 9)
                             (string-pad (number->string (msg-size m)) 7)
                             (one-line preview 64)))
          (loop (cdr ms) (+ i 1)))))))

(def (show-history)
  (for-each (lambda (m)
              (displayln (format "\n--- ~a~a ---\n~a" (msg-role m)
                                 (if (pair? (msg-tool-names m))
                                   (format " (tool calls: ~a)" (string-join (msg-tool-names m) ", "))
                                   "")
                                 (message-content m))))
            (messages)))

(def (compact-context)
  (let ((msgs (messages)))
    (if (<= (length msgs) 2)
      (displayln "Nothing to compact -- the conversation is already short.")
      (let* ((before (apply + (map msg-size msgs)))
             (transcript
              (string-join
               (map (lambda (m)
                      (format "### ~a~a\n~a" (msg-role m)
                              (if (pair? (msg-tool-names m))
                                (format " (tool calls: ~a)" (string-join (msg-tool-names m) ", ")) "")
                              (message-content m)))
                    (cdr msgs))
               "\n\n")))
        (displayln (format "Compacting ~a messages (~a chars)..." (length msgs) before))
        (try
         (let ((summary (llm-chat [(hash (role "system") (content COMPACT-SYSTEM-PROMPT))
                                   (hash (role "user") (content transcript))])))
           (messages [(car msgs)
                      (hash (role "user")
                            (content (string-append "[Earlier conversation compacted to this summary. Continue from where it left off.]\n\n"
                                                    summary)))])
           (displayln (format "Compacted: ~a -> ~a chars." before (apply + (map msg-size (messages)))))
           (show-context))
         (catch (e) (displayln (format "Compaction failed: ~a" (error-text e)))))))))

(def (error-text e)
  (if (error-object? e)
    (let ((irritants (error-object-irritants e)))
      (if (null? irritants)
        (error-object-message e)
        (format "~a ~a" (error-object-message e) irritants)))
    (format "~a" e)))

;; ---------------------------------------------------------------------------
;; Banner and help

(def (print-banner)
  (unless (quiet?)
    (displayln "")
    (displayln (colorize BOLD (format "Coding Harness ~a  --  /help for commands, /quit to exit" VERSION)))
    (displayln (format "  cwd:      ~a" (path-expand (current-directory))))
    (displayln (format "  language: ~a" (language-title (current-lang))))
    (displayln (format "  provider: ~a" (or (config-active-provider-name) "(none)")))
    (displayln (format "  model:    ~a" (current-model-id)))
    (displayln (format "  check:    ~a~a" (setting 'check_command)
                       (if (or (file-exists? "Makefile") (file-exists? "makefile")) ""
                           "  (no Makefile here: built-in check)")))
    (displayln (format "  skill:    ~a"
                       (let ((s (loaded-skill)))
                         (if s (format "~a (~a chars in the system prompt)" (car s) (cdr s)) "none"))))
    (when (auto-approve?) (displayln (colorize YELLOW "  auto-approve is ON (-y)")))
    (when (dry-run?) (displayln (colorize YELLOW "  dry-run: diffs are shown, nothing is written")))
    (displayln "")))

(def HELP-TEXT #<<END

Commands:
  /check             run the check now (syntax errors: you fix, then Enter re-runs it)
  /delims <file>     check delimiter balance of a source file (also /parens)
  /outline <file>    list the definitions in a source file
  /eval <code>       run code in the project language in this directory
  /examples <regex>  search the examples directory (examples_dir setting)
  /lang [name]       show or switch the project language
  /yes               toggle auto-approve of edits
  /reset             clear the conversation
  /history           dump the message log
  /context           summarize what is in the context window
  /compact           replace the history with a model-written summary
  /model <id>        switch model for the current provider
  /provider [name]   show or switch provider profile
  /tokens            session token usage and estimated cost
  /debug             toggle request/response logging
  /skills            list skills in ~/.agents/skills
  /<skill-name>      load that skill into the conversation
  /quit              exit
END
  )

;; ---------------------------------------------------------------------------
;; Slash commands  ->  'quit | 'continue | #f (not a command)

(def (command-arg line cmd)
  (trim (substring line (string-length cmd) (string-length line))))

(def (handle-slash-command line)
  (cond
   ((member line '("/quit" "/exit")) 'quit)
   ((not (string-prefix? "/" line)) #f)
   ((string=? line "/help") (displayln HELP-TEXT) 'continue)
   ((string=? line "/reset") (reset-conversation) (displayln "Conversation reset.") 'continue)
   ((string=? line "/history") (show-history) 'continue)
   ((string=? line "/context") (show-context) 'continue)
   ((string=? line "/compact") (compact-context) 'continue)
   ((string=? line "/tokens") (print-session-stats) 'continue)
   ((string=? line "/debug")
    (debug? (not (debug?)))
    (displayln (format "Debug logging ~a" (if (debug?) "ON" "OFF")))
    'continue)
   ((string=? line "/yes")
    (auto-approve? (not (auto-approve?)))
    (displayln (format "Auto-approve ~a" (if (auto-approve?) "ON (diffs still shown)" "OFF")))
    'continue)
   ((string=? line "/check")
    (let-values (((status msg) (check-with-user-fix (source-files-here) #f)))
      (displayln (colorize (if (memq status '(passed user-fixed)) GREEN RED) msg))
      ;; Tell the model, so its picture of the files stays current.
      (add-message! (hash (role "user") (content (format "[The user ran the check: ~a]" msg)))))
    'continue)
   ((or (string-prefix? "/delims " line) (string-prefix? "/parens " line))
    (let ((f (command-arg line (if (string-prefix? "/delims " line) "/delims" "/parens"))))
      (if (file-exists? f)
        (let-values (((ok? text) (paren-check-file f)))
          (displayln (colorize (if ok? GREEN YELLOW) text)))
        (displayln (format "No such file: ~a" f))))
    'continue)
   ((string-prefix? "/outline " line)
    (displayln (outline-file (command-arg line "/outline")))
    'continue)
   ((string-prefix? "/eval " line)
    (displayln (run-eval (command-arg line "/eval")))
    'continue)
   ((string-prefix? "/examples " line)
    (displayln (search-examples (command-arg line "/examples")))
    'continue)
   ((string-prefix? "/model " line)
    (model-override (command-arg line "/model"))
    (displayln (format "Model set to ~a" (current-model-id)))
    'continue)
   ((string=? line "/provider")
    (displayln (format "Current provider: ~a (model: ~a)" (config-active-provider-name) (current-model-id)))
    (displayln (format "Available profiles: ~a" (string-join (config-provider-names) ", ")))
    'continue)
   ((string-prefix? "/provider " line)
    (let ((p (command-arg line "/provider")))
      (if (config-provider p)
        (begin (config-set-active-provider! p)
               (model-override #f)
               (displayln (format "Provider set to '~a' (model: ~a)" p (current-model-id))))
        (displayln (format "Unknown provider '~a' -- use one of: ~a" p (string-join (config-provider-names) ", ")))))
    'continue)
   ((string=? line "/lang")
    (displayln (format "Language: ~a (~a). Available: ~a"
                       (language-name (current-lang)) (language-title (current-lang))
                       (string-join (language-names) ", ")))
    'continue)
   ((string-prefix? "/lang " line)
    (let ((lang (find-language (command-arg line "/lang"))))
      (if lang
        (begin (select-language! lang)
               (refresh-system-prompt)
               (displayln (format "Language set to ~a." (language-title lang))))
        (displayln (format "Unknown language -- use one of: ~a" (string-join (language-names) ", ")))))
    'continue)
   ((string=? line "/skills") (show-skills) 'continue)
   (else (load-skill (substring line 1 (string-length line))) 'continue)))

(def (show-skills)
  (let ((skills (list-skills)))
    (if (null? skills)
      (displayln (format "No skills found in ~a" (skills-dir)))
      (begin
        (displayln (format "Skills in ~a:" (skills-dir)))
        (for-each (lambda (s)
                    (let ((d (try (skill-description s) (catch (e) #f))))
                      (displayln (if d (format "  /~a -- ~a" s (one-line d 90)) (format "  /~a" s)))))
                  skills)))))

(def (load-skill name)
  (if (not (file-exists? (skill-file name)))
    (displayln (format "Unknown command or skill: /~a  (try /help or /skills)" name))
    (let ((content (read-file (skill-file name))))
      (add-message! (hash (role "system")
                          (content (format "The user loaded the skill '~a'. Use it as authoritative reference for the rest of this conversation.\n\n~a"
                                           name content))))
      (displayln (format "Loaded skill ~a (~a chars) into the conversation." name (string-length content))))))

;; ---------------------------------------------------------------------------
;; Talking to the model

(def (send-to-model user-line)
  (add-message! (hash (role "user") (content user-line)))
  (let-values (((reply new-messages)
                (llm-chat-with-tools (messages) (tool-schemas) execute-tool-calls
                                     (setting 'max_iterations))))
    (messages new-messages)
    (displayln (string-append "\n" (trim reply)))))

;; One-shot exit code, from what the tools reported.
(def (tool-results-contain? needle)
  (ormap (lambda (m) (and (string=? (msg-role m) "tool")
                          (string-contains (message-content m) needle)))
         (messages)))

(def (infer-exit-code)
  (cond
   ((tool-results-contain? "SYNTAX ERROR") EXIT-SYNTAX-ERROR)
   ((tool-results-contain? "FAILED") EXIT-CHECK-FAILED)
   ((or (tool-results-contain? "user rejected")
        (tool-results-contain? "user skipped")
        (tool-results-contain? "not applied")) EXIT-REJECTED)
   (else EXIT-OK)))

(def (run-one-shot prompt)
  (reset-conversation)
  (let ((code (try
               (send-to-model prompt)
               (infer-exit-code)
               (catch (e)
                 (eprintf "Error talking to model: ~a\n" (error-text e))
                 EXIT-MODEL-ERROR))))
    (force-output)
    (exit code)))

(def (run-repl)
  (reset-conversation)
  (print-banner)
  (let loop ()
    (let ((line (prompt-line (colorize (string-append BOLD GREEN) (format "\n~a> " (language-name (current-lang)))))))
      (cond
       ((eof-object? line) (newline))
       ((string=? line "") (loop))
       (else
        (case (handle-slash-command line)
          ((quit) (void))
          ((continue) (loop))
          (else
           (try
            (send-to-model line)
            (catch (e)
              (displayln (colorize RED (format "\nError talking to model: ~a" (error-text e))))))
           (loop))))))))

;; ---------------------------------------------------------------------------
;; Command line

(def USAGE #<<END
usage: gxi harness.ss [options] [prompt ...]

With a prompt, run it once and exit; with none, start the REPL.

  -p, --prompt TEXT   prompt text (repeatable; joined with blank lines)
  --stdin             read the prompt from stdin
  -y, --yes           auto-approve edits (diffs still shown; unbalanced code is never auto-written)
  --dry-run           show diffs, never write files
  --provider NAME     provider profile from the harness config
  --model ID          model id for this session
  --cwd DIR           work in DIR
  --lang NAME         project language: gerbil, python, haskell, typescript,
                      javascript, generic (default: detected from the files)
  --no-skill          do not put the language's skill in the system prompt
  --debug             log requests and responses
  -q, --quiet         no banner, less tool chatter
  --plain, --no-color no ANSI colors
  -v, --version       show version
  -h, --help          show this help

Exit codes: 0 ok, 1 model/network error, 2 check failed, 3 edit rejected,
            4 unresolved syntax error, 5 bad arguments.
END
  )

(def (die msg)
  (eprintf "error: ~a\n" msg)
  (exit EXIT-BAD-ARGS))

;; parse-args : (listof string) -> hash of options
(def (parse-args args)
  (let ((opts (hash (prompts []) (positional []))))
    (let loop ((args args))
      (unless (null? args)
        (let ((a (car args)) (rest (cdr args)))
          (def (value!)
            (when (null? rest) (die (format "~a needs a value" a)))
            (car rest))
          (cond
           ((member a '("-h" "--help")) (displayln USAGE) (exit EXIT-OK))
           ((member a '("-v" "--version")) (displayln (format "coding-harness ~a" VERSION)) (exit EXIT-OK))
           ((member a '("-p" "--prompt"))
            (hash-put! opts 'prompts (append (hash-ref opts 'prompts) [(value!)])) (loop (cdr rest)))
           ((string=? a "--provider") (hash-put! opts 'provider (value!)) (loop (cdr rest)))
           ((string=? a "--model") (hash-put! opts 'model (value!)) (loop (cdr rest)))
           ((string=? a "--cwd") (hash-put! opts 'cwd (value!)) (loop (cdr rest)))
           ((string=? a "--lang") (hash-put! opts 'lang (value!)) (loop (cdr rest)))
           ((string=? a "--stdin") (hash-put! opts 'stdin #t) (loop rest))
           ((member a '("-y" "--yes")) (hash-put! opts 'yes #t) (loop rest))
           ((string=? a "--dry-run") (hash-put! opts 'dry-run #t) (loop rest))
           ((string=? a "--no-skill") (hash-put! opts 'no-skill #t) (loop rest))
           ((string=? a "--debug") (hash-put! opts 'debug #t) (loop rest))
           ((member a '("-q" "--quiet")) (hash-put! opts 'quiet #t) (loop rest))
           ((member a '("--plain" "--no-color")) (hash-put! opts 'plain #t) (loop rest))
           ((and (string-prefix? "-" a) (> (string-length a) 1)) (die (format "unknown option ~a (try --help)" a)))
           (else (hash-put! opts 'positional (append (hash-ref opts 'positional) [a])) (loop rest))))))
    opts))

(def (main . args)
  (let ((opts (parse-args args)))
    ;; --cwd first, so .local_coding_harness.json is read from the project.
    (let ((dir (hash-get opts 'cwd)))
      (when dir
        (unless (file-directory? dir) (die (format "--cwd directory does not exist: ~a" dir)))
        (current-directory (path-expand dir))))
    (load-harness-config)
    (when (null? (config-provider-names))
      (die (format "no providers configured. Add a \"providers\" section to ~a or ~a (see README.md)."
                   (global-config-path) (local-config-path))))
    ;; Language: --lang, then "language" in the config, then detection.
    (let* ((name (or (hash-get opts 'lang) (json-ref (harness-config) 'language)))
           (lang (if name
                   (or (find-language name)
                       (die (format "unknown language '~a' -- available: ~a" name (string-join (language-names) ", "))))
                   (detect-language))))
      (select-language! lang))
    (let ((cfg (harness-config)))
      (when (json-ref cfg 'quiet) (quiet? #t))
      (when (json-ref cfg 'plain) (color? #f))
      (when (json-ref cfg 'debug) (debug? #t)))
    (when (hash-get opts 'quiet) (quiet? #t))
    (when (hash-get opts 'plain) (color? #f))
    (when (hash-get opts 'debug) (debug? #t))
    (when (hash-get opts 'yes) (auto-approve? #t))
    (when (hash-get opts 'dry-run) (dry-run? #t))
    (when (hash-get opts 'no-skill) (use-skill? #f))
    (let ((p (hash-get opts 'provider)))
      (when p
        (unless (config-provider p)
          (die (format "unknown provider '~a' -- available: ~a" p (string-join (config-provider-names) ", "))))
        (config-set-active-provider! p)))
    (let ((m (hash-get opts 'model)))
      (when m (model-override m)))
    (let* ((stdin-text (if (hash-get opts 'stdin) (read-all-as-string (current-input-port)) ""))
           (parts (filter (lambda (s) (not (string=? (trim s) "")))
                          (append [stdin-text] (hash-ref opts 'prompts) (hash-ref opts 'positional))))
           (prompt (trim (string-join parts "\n\n"))))
      ;; Questions (approve? fix the syntax error?) need a person on stdin.
      (interactive? (and (not (hash-get opts 'stdin)) (##tty? (current-input-port))))
      (if (string=? prompt "")
        (run-repl)
        (run-one-shot prompt)))))
