# Building an Autonomous LLM Coding Harness

Artificial intelligence coding agents have emerged as one of the most transformative applications of large language models. Rather than operating as static question-answering systems, modern coding agents act autonomously: they inspect a codebase, search for relevant definitions, inspect existing tests, propose structured file modifications, run verification checks, and iterate until a task is solved.

Yet building a reliable autonomous coding assistant reveals several critical engineering challenges:

1. **Unconstrained Writes and Code Regression**: A model left to overwrite files without human oversight can silently delete existing logic or introduce subtle regressions.
2. **The Delimiter Counting Blind Spot**: Neural language models process text as discrete tokens rather than nested syntax trees. When a model introduces an unclosed parenthesis or bracket, the compiler often emits an unhelpful diagnostic with zero location information (for example, `Incomplete form, EOF reached`). If you feed this error back to the model, it typically rewrites the entire file from scratch, shifting the parenthesis imbalance somewhere else.
3. **Execution Safety and Sandboxing**: A model generating code must be able to run tests and evaluate expressions, but it must not execute arbitrary shell commands that could compromise the host environment.

In this chapter, we construct a complete, multi-language coding agent in Gerbil Scheme called **Coding Harness**. The harness implements an interactive tool-calling loop, generates colored unified diffs for every proposed edit, intercepts compiler errors with an automated delimiter scanner, and prompts the developer to fix bracket mistakes directly in their editor.

## The Architecture of an Agentic Harness

An effective coding agent is organized as a stateful loop coordinating four specialized layers:

```
┌─────────────────────────────────────────────────────────────┐
│                      User Objective                         │
└──────────────────────────────┬──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                LLM Tool-Calling Loop (llm.ss)               │
│                                                             │
│   • Multi-turn conversation context                         │
│   • OpenAI-compatible Chat Completions API                  │
│   • Function calling & tool dispatch                        │
└──────────────┬──────────────────────────────▲───────────────┘
               │                              │
        Tool Invocations                 Tool Results
               │                              │
               ▼                              │
┌─────────────────────────────────────────────────────────────┐
│                 Tool Registry (tools.ss)                    │
│                                                             │
│   • read_file, list_dir, grep, outline_file, find_definition│
│   • eval_code (sandboxed REPL execution)                    │
│   • run_shell (whitelisted commands)                        │
│   • propose_edit (staged file modification)                 │
└──────────────┬──────────────────────────────▲───────────────┘
               │                              │
          Staged Edit                    Check Status
               │                              │
               ▼                              │
┌─────────────────────────────────────────────────────────────┐
│              Approval & Verification Gate                   │
│                                                             │
│   1. approval.ss: Display colored diff & request approval   │
│   2. checker.ss:  Run build/test verification               │
│   3. parens.ss:   Scan delimiters on syntax errors          │
└─────────────────────────────────────────────────────────────┘
```

### Why Syntax Errors Go to the User

When a compiler cannot parse a file due to an unclosed delimiter, it usually cannot identify which opening bracket lacked a matching closer. In Gerbil, the compiler simply says:

```text
--- Syntax Error at read-module: error reading module
... detail: "Incomplete form, EOF reached\n"
```

Because language models lack a mechanical stack, handing this message back to the model rarely works. The model guesses where the missing parenthesis goes, rewrites dozens of lines, and frequently creates a new imbalance.

The Coding Harness inverts this dynamic. When `make check` fails, the harness runs a lexical delimiter scanner (`parens.ss`). The scanner tracks every opening and closing delimiter while respecting string literals, block comments, and escape codes. It pinpoints the exact line and column of the unclosed delimiter, prints an excerpt of the code with a visual pointer, and pauses to let the user fix the file in their editor. Once the developer saves the file and presses Enter, the harness re-runs the check and resumes the agent.

## The Delimiter Scanner (`parens.ss`)

The module `parens.ss` implements lexical token scanners for four distinct language families:
- `lisp`: Handles Scheme and Gerbil strings, vertical-bar symbols, line comments (`;`), nested block comments (`#| ... |#`), and here-strings (`#<<`).
- `python`: Handles hash comments (`#`), single quotes, double quotes, and triple-quoted multiline strings.
- `haskell`: Handles line comments (`--`), nested block comments (`{- ... -}`), and character literals.
- `c-like`: Handles line comments (`//`), block comments (`/* ... */`), single quotes, double quotes, template literals (`` `...` ``), and regular expression literals (`/.../`).

Here are the key scanning and diagnostic reporting procedures from `parens.ss`:

```scheme
;; File: parens.ss (excerpts)
;; Lexical delimiter scanner that reports unbalanced (), [], and {}

(import :std/sugar
        :std/format
        :std/srfi/13)

(export scan-parens
        scan-delimiters
        parens-ok?
        format-paren-report
        source-excerpt)

(defstruct paren-problem
  (kind line col message closer opener-line opener-col))

(defstruct paren-report
  (ok? problems hints forms))

;; Scan TEXT using LEXICON rules ('lisp, 'python, 'haskell, or 'c-like)
(def (scan-delimiters text (lexicon 'lisp))
  (let ((stack '())          ;; stack of (char line col)
        (problems '())
        (hints '())
        (line 1)
        (col 1)
        (len (string-length text)))
    (let loop ((i 0))
      (if (>= i len)
        ;; Reached EOF: check if any opening delimiters remain on stack
        (begin
          (for-each
           (lambda (entry)
             (let ((ch (car entry))
                   (l (cadr entry))
                   (c (caddr entry)))
               (set! problems
                     (cons (make-paren-problem
                            'unclosed l c
                            (format "'~a' opened at line ~a, col ~a is never closed (EOF reached)"
                                    ch l c)
                            #f l c)
                           problems))))
           stack)
          (make-paren-report (null? problems) (reverse problems) hints '()))
        (let ((ch (string-ref text i)))
          (cond
           ;; Opening delimiters: push onto stack
           ((memv ch '(#\( #\[ #\{))
            (set! stack (cons (list ch line col) stack))
            (advance-and-loop (+ i 1) ch line col))
           ;; Closing delimiters: verify matching opener
           ((memv ch '(#\) #\] #\}))
            (if (null? stack)
              (begin
                (set! problems
                      (cons (make-paren-problem
                             'unexpected-close line col
                             (format "Unexpected closing '~a' with no matching opener" ch)
                             ch line col)
                            problems))
                (advance-and-loop (+ i 1) ch line col))
              (let* ((top (car stack))
                     (open-ch (car top))
                     (open-l (cadr top))
                     (open-c (caddr top)))
                (set! stack (cdr stack))
                (unless (matching-pair? open-ch ch)
                  (set! problems
                        (cons (make-paren-problem
                               'mismatch line col
                               (format "Mismatched closing '~a' for '~a' opened at line ~a, col ~a"
                                       ch open-ch open-l open-c)
                               ch open-l open-c)
                              problems)))
                (advance-and-loop (+ i 1) ch line col))))
           ;; Newlines: increment line counter, reset column
           ((char=? ch #\newline)
            (set! line (+ line 1))
            (set! col 1)
            (loop (+ i 1)))
           (else
            (set! col (+ col 1))
            (loop (+ i 1)))))))))
```

When a syntax error occurs, `format-paren-report` extracts the source text around the problem line and prints a pointer directly below the offending column.

## Generating Unified Diffs and Human Approval (`approval.ss`)

The module `approval.ss` ensures that no proposed modification is applied to disk without explicit review. It calculates unified diffs, renders additions in green and deletions in red, and prompts the user for action:

```scheme
;; File: approval.ss (excerpts)
;; Colored unified diff generation and interactive user approval

(import :std/sugar
        :std/format
        "util")

(export ask-approval
        render-diff)

;; Prompt the developer to approve, reject, or inspect a proposed edit
(def (ask-approval path old-content new-content)
  (let ((diff (render-diff path old-content new-content)))
    (displayln diff)
    (let loop ()
      (display "Approve this edit? [y]es, [n]o, [d]iff, [a]bort: ")
      (flush-output-port)
      (let ((reply (read-line)))
        (cond
         ((eof-object? reply) 'abort)
         ((member reply '("y" "yes" "Y")) 'approve)
         ((member reply '("n" "no" "N"))   'reject)
         ((member reply '("a" "abort" "A")) 'abort)
         ((member reply '("d" "diff" "D"))
          (displayln diff)
          (loop))
         (else
          (displayln "Please enter 'y', 'n', 'd', or 'a'.")
          (loop)))))))
```

This gate gives the developer total control over file mutations. If an edit introduces unwanted changes or alters unrelated files, the user enters `n` to reject it. The harness returns an explanation to the model so it can formulate an alternative approach.

## The Verification Gate (`checker.ss`)

After an approved edit is written to disk, `checker.ss` executes the project's build and test command (`make check` by default). If the check succeeds, control returns to the model. If the check fails, the harness analyzes the compiler output:

```scheme
;; File: checker.ss (excerpts)
;; The make check gate and user-repair loop

(import :std/sugar
        :std/format
        "util"
        "parens"
        "approval")

(export run-check
        check-with-user-fix)

(def (check-with-user-fix edited-files)
  (let-values (((output exit-code label) (run-check edited-files)))
    (if (zero? exit-code)
      (values #t output)
      ;; Check failed: inspect edited files for delimiter problems
      (let ((delims-failed #f))
        (for-each
         (lambda (file)
           (when (file-exists? file)
             (let ((rep (scan-delimiters (read-file-string file))))
               (unless (paren-report-ok? rep)
                 (set! delims-failed #t)
                 (displayln "\n━━ SYNTAX ERROR: Unbalanced Delimiters Detected ━━")
                 (displayln (format-paren-report file rep))))))
         edited-files)
        (if delims-failed
          (prompt-user-to-repair edited-files)
          (values #f output))))))

(def (prompt-user-to-repair edited-files)
  (displayln "\nPlease fix this syntax error in your editor.")
  (displayln "  [Enter]  I fixed it: re-run the check")
  (displayln "  r        Revert this edit")
  (displayln "  m        Let the model try to fix it")
  (displayln "  a        Abort this task")
  (let loop ()
    (display "fix> ")
    (flush-output-port)
    (let ((line (read-line)))
      (cond
       ((or (eof-object? line) (string=? line "a"))
        (values #f "User aborted task."))
       ((string=? line "r")
        'revert)
       ((string=? line "m")
        (values #f "Compiler syntax error: please repair."))
       (else
        ;; Re-run check after manual user fix
        (check-with-user-fix edited-files))))))
```

This interaction eliminates the loops where models repeatedly fail on bracket closures. The developer fixes the delimiter with their editor's parenthesis matching in seconds, presses Enter, and the harness continues without losing context.

## Running the Coding Harness

The harness is executed directly from the terminal inside any project directory:

```console
$ gerbil-harness "Add a helper function to format negative currency amounts"
Detected project language: gerbil (via gerbil.pkg)
Loaded 9 tools into agent registry.

=== Plan ===
1. Read source_code/currency.ss to understand existing formatting helpers.
2. Propose edit adding format-negative-currency with tests.
3. Run make check to verify.

[Tool: read_file path: "currency.ss"]
  Read 42 lines.

[Tool: propose_edit path: "currency.ss"]
--- currency.ss
+++ currency.ss
@@ -14,6 +14,11 @@
 (def (format-currency amount)
   (format "$~a" (fixed amount 2)))
+
+(def (format-negative-currency amount)
+  (if (< amount 0)
+    (format "($~a)" (fixed (abs amount) 2))
+    (format-currency amount)))

Approve this edit? [y]es, [n]o, [d]iff, [a]bort: y
[Applied edit to currency.ss]

==> Running make check...
✓ Check PASSED (all tests OK).

Task complete. The new helper format-negative-currency is implemented and passing tests.
```

Now let us observe what happens when a model proposes an edit containing an unbalanced delimiter:

```console
[Tool: propose_edit path: "util.ss"]
--- util.ss
+++ util.ss
@@ -3,4 +3,5 @@
 (def (add1* x) (+ x 1))
+(def (add2 x)
+  (let ((y (add1* x)))
+    (+ y 1))

Approve this edit? [y]es, [n]o, [d]iff, [a]bort: y
[Applied edit to util.ss]

==> Running make check...

━━ SYNTAX ERROR: Unbalanced Delimiters Detected ━━
util.ss: UNBALANCED DELIMITERS

* '(' opened at line 4, col 1 is never closed (EOF reached)
      3| (def (add1* x) (+ x 1))
      4| (def (add2 x)
         ^
      5|   (let ((y (add1* x)))
      6|     (+ y 1))

hint: missing 1 closing delimiter ')' at end of form.

Please fix this syntax error in your editor.
  [Enter]  I fixed it: re-run the check
  r        Revert this edit
  m        Let the model try to fix it
  a        Abort this task
fix> 
```

After appending the missing `)` in your editor and pressing Enter, the harness detects the clean check and proceeds smoothly:

```console
==> Running make check...
✓ Check PASSED.
Resuming agent workflow.
```

## Interpreting the Results

The interaction highlights three critical architectural advantages:

1. **Human Oversight of Mutations**: By surfacing proposed modifications as unified diffs before disk writes, developers prevent unauthorized deletions, hallucinated library dependencies, or out-of-scope edits.
2. **Deterministic Bracket Correction**: Language models struggle with character-level delimiter balancing. By transferring reader errors to the user alongside visual line and column excerpts, the harness eliminates the primary failure mode of automated coding assistants.
3. **Multi-Language Adaptability**: Because the delimiter lexicons and syntax error rules are separated into language profiles (`lang.ss`), the exact same harness operates across Gerbil, Python, Haskell, TypeScript, and JavaScript projects.

## Wrap Up

In this chapter we built Coding Harness, an autonomous agentic pair-programming assistant in Gerbil Scheme. By combining multi-turn tool calling, colored unified diff approval, automated build verification, and a dedicated delimiter scanner, the system bridges the gap between language model generation and software engineering rigor.

## Optional Practice Problems

1. **Automated Rollback on Abort**: Extend `approval.ss` to maintain an in-memory backup of every file touched during a session, automatically reverting all disk mutations if the user selects `abort`.
2. **Context Compaction**: Implement a `/compact` command in `llm.ss` that summarizes earlier turns in the conversation history when token consumption exceeds 80% of the model's context window.
3. **AST Definition Finder**: Extend `outline_file` in `tools.ss` to use Gerbil's macro expander (`gxc -S`) to report fully qualified export lists and macro definitions in addition to top-level `def` forms.
