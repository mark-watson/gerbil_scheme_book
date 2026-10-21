;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; lang.ss -- language profiles: everything the harness knows about one
;;; programming language, and how it picks the language of a project.
;;;
;;; A profile holds:
;;;   - file extensions, and the lexicon the delimiter scanner reads them with
;;;   - the built-in check (used when there is no Makefile `check` target)
;;;   - regexps that classify compiler output as a syntax error
;;;   - grep patterns for find_definition and outline_file
;;;   - how eval_code runs a snippet
;;;   - extra run_shell commands, a system-prompt section, setting defaults
;;;
;;; Profiles: gerbil, python, haskell, typescript, javascript, and generic
;;; (no language knowledge, used when nothing is detected). The project
;;; language comes from --lang, then "language" in the config, then marker
;;; files (gerbil.pkg, *.cabal, tsconfig.json, pyproject.toml, ...), then the
;;; most common source extension. Per-file work (delimiter scans, the
;;; built-in check) always goes by each file's own extension, so a mixed
;;; project is checked correctly whatever the project language is.

(import :std/sugar
        :std/format
        :std/sort
        :std/srfi/13
        :std/pregexp
        "config"
        "util"
        "parens")

(export (struct-out language)
        all-languages
        language-names
        find-language
        current-lang
        select-language!
        detect-language
        language-for-path
        lexicon-for-path
        source-path?
        project-source-path?
        source-files-here
        source-extensions-all
        eval-command
        BASE-SHELL-COMMANDS
        shell-commands)

(defstruct language
  (name               ; "python"
   title              ; "Python"
   extensions         ; (".py" ".pyi")
   lexicon            ; scanner lexicon, or #f for no delimiter scan
   strict?            ; #t: -y never writes unbalanced code (Lisp)
   setting-defaults   ; hash of setting defaults (skill, ask_user_on, ...)
   prompt             ; system-prompt section with language idioms
   def-pattern        ; grep -E pattern for a definition; each ~a is the name
   outline-pattern    ; grep -E pattern for outline lines, or #f: Lisp forms
   reader-patterns    ; pregexps: output means unbalanced/unreadable source
   syntax-patterns    ; pregexps: output means some other syntax error
   reader-advice      ; said when the compiler cannot read a balanced file
   file-check         ; (path tmpdir) -> args or #f, for the built-in check
   project-check      ; () -> args or #f; replaces per-file checks when set
   evaluator          ; (code tmpdir) -> args, or #f: no eval_code tool
   eval-help          ; eval_code parameter description
   shell))            ; extra run_shell command prefixes

;; ---------------------------------------------------------------------------
;; Helpers for the profiles

(def (file-matching? pred) (find pred (directory-files ".")))

(def (has-ext? ext) (lambda (f) (string-suffix? ext f)))

(def (python-exe)
  (cond ((file-exists? ".venv/bin/python") ".venv/bin/python")
        ((file-exists? "venv/bin/python") "venv/bin/python")
        (else "python3")))

;; Parse a Python file without running or byte-compiling it, and report a
;; SyntaxError as file:line:col so the checker can show an excerpt.
(def PY-PARSE #<<END
import ast, sys
p = sys.argv[1]
try:
    ast.parse(open(p, encoding="utf-8").read(), p)
except SyntaxError as e:
    print(f"{e.filename}:{e.lineno}:{e.offset or 1}: {type(e).__name__}: {e.msg}")
    print(e.text or "")
    sys.exit(1)
END
  )

;; Run Python code; if the last statement is an expression, print its repr
;; (like the REPL does).
(def PY-EVAL #<<END
import ast, sys
sys.path.insert(0, "")
tree = ast.parse(sys.argv[1], "<eval>")
last = None
if tree.body and isinstance(tree.body[-1], ast.Expr):
    last = ast.Expression(tree.body.pop().value)
env = {"__name__": "__main__"}
exec(compile(tree, "<eval>", "exec"), env)
if last is not None:
    value = eval(compile(last, "<eval>", "eval"), env)
    if value is not None:
        print(repr(value))
END
  )

;; Gerbil: gxi -e takes one form per flag, so split the code into top-level
;; forms; if the last form is an expression, print its value with `write`.
(def NON-VALUE-HEADS '("def" "define" "import" "export" "defstruct" "defclass" "defrule" "defrules"
                       "defsyntax" "defmethod" "defvalues" "displayln" "display" "write" "pretty-print"
                       "for-each" "begin" "set!" "hash-put!"))

(def (form-head form)
  (let* ((s (substring form 1 (string-length form)))
         (end (or (string-index s (lambda (c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\]))))) (string-length s))))
    (substring s 0 end)))

(def (gerbil-eval-args code tmpdir)
  (let* ((forms (top-level-form-strings code))
         (forms (if (null? forms) [(trim code)] forms))
         (last-form (last forms))
         (printed (if (and (string-prefix? "(" last-form)
                           (not (member (form-head last-form) NON-VALUE-HEADS)))
                    (format "(let ((v ~a)) (unless (void? v) (write v) (newline)))" last-form)
                    last-form)))
    (cons "gxi" (append-map (lambda (f) ["-e" f]) (append (drop-last forms) [printed])))))

;; Haskell: the snippet is split into top-level chunks (a chunk starts at a
;; line in column 1). The leading chunks become a scratch module; the last
;; one, if it is an expression, is evaluated with `ghc -e`. A snippet that
;; defines main is run as a program.
(def HASKELL-DECLARATION
  "^(import|data|newtype|type|class|instance|module|deriving|infix|\\{-|--)|^[a-z_][A-Za-z0-9_']*[^=]*(::|[^=<>/!:]=[^=>])")

(def (haskell-chunks code)
  (let loop ((lines (string-split code #\newline)) (cur []) (acc []))
    (def (flush) (if (null? cur) acc (cons (string-join (reverse cur) "\n") acc)))
    (cond
     ((null? lines) (reverse (flush)))
     ((string=? (trim (car lines)) "") (loop (cdr lines) cur acc))
     ((char-whitespace? (string-ref (car lines) 0)) (loop (cdr lines) (cons (car lines) cur) acc))
     (else (loop (cdr lines) [(car lines)] (flush))))))

(def (haskell-eval-args code tmpdir)
  (let* ((chunks (haskell-chunks code))
         (decl? (lambda (c)
                  (and (pregexp-match HASKELL-DECLARATION c)
                       (not (pregexp-match "^(let|if|case|do|print|putStrLn)[[:space:]]" c)))))
         (main? (find (lambda (c) (pregexp-match "^main[[:space:]]*(::|=)" c)) chunks))
         (expr (cond (main? "main")
                     ((and (pair? chunks) (not (decl? (last chunks)))) (last chunks))
                     (else "putStrLn \"(loaded; no expression to evaluate)\"")))
         (decls (if (or main? (null? chunks) (decl? (last chunks))) chunks (drop-last chunks)))
         (search ["-i." "-isrc" "-iapp" "-ilib"]))
    (if (null? decls)
      (append ["ghc" "-e" expr] search)
      (let ((file (path-expand "Eval.hs" tmpdir)))
        (write-file file (string-append (string-join decls "\n") "\n"))
        (append ["ghc" "-e" expr] search [file])))))

(def (tsc) (find-executable "tsc"))

;; ---------------------------------------------------------------------------
;; The profiles

(def BASE-SHELL-COMMANDS '("make" "ls" "pwd" "cat" "head" "tail" "wc" "diff"))

(def GENERAL-RULES #<<END
- Delimiters: every ( [ { closes with the matching ) ] }. Before calling propose_edit, check that the closers at the end of each block or function match what you opened. Unbalanced code is shown to the user with a warning.
END
  )

(def gerbil
  (make-language
   "gerbil" "Gerbil Scheme"
   '(".ss" ".scm" ".sld" ".ssi" ".ssxi" ".sls")
   'lisp #t
   (hash (skill "gerbil-expert")
         (examples_dir "~/GITHUB/gerbil_scheme_book/source_code")
         (ask_user_on "syntax"))
   #<<END
Writing correct Gerbil Scheme (v0.18, built on Gambit):
- Delimiters: every ( [ { closes with the matching ) ] }. Keep functions short and nesting shallow, finish each top-level form before starting the next, and count the closers at the end of every top-level form before calling propose_edit. Unbalanced code is shown to the user as an error.
- Unsure of a :std library function's module, name or arguments? Look it up with search_examples or try it with eval_code BEFORE writing code that depends on it.
- File layout: (import ...) then (export ...) then definitions. Local modules: (import "foo") for ./foo.ss. Common standard modules: :std/format (format, printf), :std/sugar (try/catch/finally, hash, while), :std/iter (for, in-range), :std/text/json, :std/net/request, :std/misc/ports (read-file-string), :std/srfi/13 (string-prefix?, string-contains, string-trim-both), :std/sort, :std/pregexp.
- Define with def: (def (f x (opt 1) key: (key "default")) ...). defstruct for records.
- Hash tables: make-hash-table, hash-put!, hash-ref (error on a missing key unless given a default), hash-get (#f when missing). Parsed JSON objects have symbol keys and JSON null reads as #!void.
- [a b c] is list syntax; {method obj} is method-call syntax.
- srfi-13 string-trim trims only the left side; use string-trim-both.
- A runnable script does (export main) and (def (main . args) ...); run it with `gxi file.ss args`.
- Projects should have a Makefile whose `check` target verifies the code (for example `gxc -S -d .gerbil/check *.ss` plus tests) so every edit is checked.
END
   "\\((def|define|def\\*|defvalues|defconst|defalias|definline|defstruct|defclass|defmethod|defrule|defrules|defsyntax|defsyntax-call|define-syntax|defgeneric|deferror-class|define-values|defcall-actor|defmessage)[[:space:]]+[(]?~a([[:space:])]|$)"
   #f
   '("Incomplete form" "Datum or EOF expected" "error reading module" "Datum expected" "Invalid token" "Unbalanced")
   '("Syntax Error")
   "The Gerbil reader could not read a file, but every delimiter balances. Look for a stray '#', a bad character literal (#\\x...), or an unterminated #| comment."
   (lambda (path tmpdir) ["gxc" "-S" "-d" tmpdir path])
   (lambda () #f)
   gerbil-eval-args
   "One or more Gerbil forms. (import \"foo\") loads ./foo.ss. The last expression's value is printed."
   '("gxc" "gxpkg" "gerbil")))

(def python
  (make-language
   "python" "Python"
   '(".py" ".pyi")
   'python #f
   (hash)
   (string-append GENERAL-RULES #<<END
Writing good Python:
- Follow the style already in the project (formatter, quotes, typing). New code: Python 3.10+, type hints on public functions, f-strings, pathlib, dataclasses for records.
- Prefer the standard library; do not add a dependency without asking.
- Indentation is syntax: 4 spaces, never tabs; keep the indentation of untouched lines exactly as it was.
- Tests use pytest (tests/test_*.py) unless the project already uses something else.
- Use eval_code to check how a library call behaves before relying on it.
END
     )
   "^[[:space:]]*(async[[:space:]]+)?(def|class)[[:space:]]+~a([^A-Za-z0-9_]|$)|^~a[[:space:]]*(:[^=]*)?=[^=]"
   "^[[:space:]]*(async[[:space:]]+)?(def|class)[[:space:]]"
   '("was never closed" "unmatched '" "does not match opening parenthesis"
     "unterminated string literal" "unterminated triple-quoted string" "EOF while scanning")
   '("SyntaxError" "IndentationError" "TabError")
   "Python could not parse a file, but every delimiter balances. Look for bad indentation, a missing ':' or an unclosed string prefix."
   (lambda (path tmpdir) [(python-exe) "-c" PY-PARSE path])
   (lambda () #f)
   (lambda (code tmpdir) [(python-exe) "-c" PY-EVAL code])
   "Python statements. The project directory is on sys.path, so `import mymodule` works. If the last statement is an expression its repr is printed."
   '("pytest" "python3 -m pytest" "python -m pytest" "uv run pytest" "ruff check" "mypy" "pyright")))

(def haskell
  (make-language
   "haskell" "Haskell"
   '(".hs" ".hs-boot")
   'haskell #f
   (hash)
   (string-append GENERAL-RULES #<<END
Writing good Haskell (GHC):
- Give every top-level binding a type signature. Avoid partial functions (head, tail, fromJust, !!); pattern match or use Maybe.
- Layout is syntax: align the bodies of do, where, let, case and guards consistently, with spaces only.
- Import containers qualified (import qualified Data.Map.Strict as Map); prefer Text over String for real text.
- Add a dependency to the .cabal / package.yaml file only after asking.
- Use eval_code to check a function's type or behaviour before relying on it.
END
     )
   "^[[:space:]]*~a[[:space:]]+::|^~a([[:space:]][^=]*)?[[:space:]]=([^=]|$)|^(data|newtype|type|class)[[:space:]]+(.*=>[[:space:]]*)?~a([^A-Za-z0-9_']|$)"
   "^(module|import|data|newtype|type|class|instance)[[:space:]]|^[a-z_][A-Za-z0-9_']*[[:space:]]*::"
   '("unterminated `\\{-'" "lexical error in string" "mismatched brackets")
   '("parse error" "lexical error")
   "GHC could not parse a file, but every delimiter balances. Look at the layout: a line indented less (or more) than the block it belongs to."
   (lambda (path tmpdir) ["ghc" "-fno-code" "-v0" "-i." "-isrc" "-iapp" "-ilib" path])
   (lambda ()
     (cond ((file-exists? "stack.yaml") ["stack" "build" "--fast"])
           ((or (file-exists? "cabal.project") (file-matching? (has-ext? ".cabal"))) ["cabal" "build" "-v0"])
           (else #f)))
   haskell-eval-args
   "Haskell declarations (imports, functions) followed by one expression to evaluate and print, or a program that defines main. Project modules under ., src/, app/ and lib/ can be imported."
   '("cabal build" "cabal test" "stack build" "stack test" "hlint" "ghc -fno-code")))

(def TS-PROMPT #<<END
Writing good TypeScript/JavaScript:
- Follow the project's module system, formatter and lint rules. New code: ES modules, const by default, async/await rather than raw promise chains.
- TypeScript: keep strict mode happy; no `any` unless unavoidable (use `unknown` and narrow); export explicit types for public functions.
- Do not add an npm dependency without asking.
- Use eval_code to check how an API behaves before relying on it.
END
  )

(def JS-SHELL '("tsc" "npx tsc" "npm test" "npm run" "npx eslint" "npx vitest run" "npx jest" "node --check"))

(def typescript
  (make-language
   "typescript" "TypeScript"
   '(".ts" ".mts" ".cts" ".tsx")
   'c-like #f
   (hash)
   (string-append GENERAL-RULES TS-PROMPT)
   "(function\\*?|class|interface|type|enum|namespace|const|let|var)[[:space:]]+~a([^A-Za-z0-9_$]|$)|^[[:space:]]*(public|private|protected|static|async|[[:space:]])*~a[[:space:]]*(<[^>]*>)?\\("
   "^[[:space:]]{0,4}(export[[:space:]]+)?(default[[:space:]]+)?(declare[[:space:]]+)?(abstract[[:space:]]+)?(async[[:space:]]+)?(function|class|interface|type|enum|namespace|const|let|var)[[:space:]]"
   '("TS1002" "TS1010" "TS1160" "TS1005: '[)}\\]]' expected")
   '("error TS1[0-9][0-9][0-9]")
   "tsc could not parse a file, but every delimiter balances."
   (lambda (path tmpdir)
     (let ((t (tsc)))
       (and t (not (file-exists? "tsconfig.json"))
            [t "--noEmit" "--skipLibCheck" "--strict" "--target" "es2022" "--module" "nodenext"
               "--moduleResolution" "nodenext" "--pretty" "false" path])))
   (lambda () (let ((t (tsc))) (and t (file-exists? "tsconfig.json") [t "--noEmit" "--pretty" "false" "-p" "."])))
   (lambda (code tmpdir) ["node" "-p" code])
   "TypeScript or JavaScript run with `node -p` (Node 23.6+ strips types); the value of the last expression is printed."
   JS-SHELL))

(def javascript
  (make-language
   "javascript" "JavaScript"
   '(".js" ".mjs" ".cjs" ".jsx")
   'c-like #f
   (hash)
   (string-append GENERAL-RULES TS-PROMPT)
   "(function\\*?|class|const|let|var)[[:space:]]+~a([^A-Za-z0-9_$]|$)|^[[:space:]]*(static|async|[[:space:]])*~a[[:space:]]*\\("
   "^[[:space:]]{0,4}(export[[:space:]]+)?(default[[:space:]]+)?(async[[:space:]]+)?(function|class|const|let|var)[[:space:]]"
   '("Unexpected end of input" "missing \\) after argument list" "Unterminated" "Invalid or unexpected token")
   '("SyntaxError")
   "Node could not parse a file, but every delimiter balances."
   (lambda (path tmpdir) (and (not (string-suffix? ".jsx" path)) ["node" "--check" path]))
   (lambda () #f)
   (lambda (code tmpdir) ["node" "-p" code])
   "JavaScript run with `node -p`; the value of the last expression is printed."
   JS-SHELL))

(def generic
  (make-language
   "generic" "any language"
   '()
   #f #f
   (hash)
   GENERAL-RULES
   "(def|define|defn|defun|function|fn|func|fun|sub|class|struct|enum|trait|interface|type|module|let|const|var|val)[[:space:]]+~a([^A-Za-z0-9_]|$)"
   "^(pub[[:space:]]+)?(def|define|defn|defun|function|fn|func|fun|sub|class|struct|enum|trait|interface|type|module|impl)[[:space:]]"
   '()
   '()
   "The compiler could not parse a file."
   (lambda (path tmpdir) #f)
   (lambda () #f)
   #f
   ""
   '()))

(def all-languages [gerbil python haskell typescript javascript generic])

(def (language-names) (map language-name all-languages))

(def (find-language name)
  (find (lambda (l) (string=? (language-name l) name)) all-languages))

;; ---------------------------------------------------------------------------
;; The current language

(def current-lang (make-parameter generic))

(def (select-language! lang)
  (current-lang lang)
  (current-language (language-name lang))
  (language-defaults (language-setting-defaults lang)))

;; ---------------------------------------------------------------------------
;; Per-file language

(def (language-for-path path)
  (let ((ext (path-extension path)))
    (and (not (string=? ext ""))
         (find (lambda (l) (member ext (language-extensions l))) all-languages))))

;; JSX/TSX text content is not code, so those files are not scanned.
(def (lexicon-for-path path)
  (let ((l (language-for-path path)))
    (and l (not (member (path-extension path) '(".tsx" ".jsx"))) (language-lexicon l))))

(def (source-path? path) (and (language-for-path path) #t))

(def (project-source-path? path)
  (member (path-extension path) (language-extensions (current-lang))))

(def (source-extensions-all) (append-map language-extensions all-languages))

;; Source files of the project language in the working directory (for /check).
(def (source-files-here)
  (sort (filter project-source-path? (directory-files ".")) string<?))

;; ---------------------------------------------------------------------------
;; Detection

(def MARKERS
  [["gerbil.pkg" "gerbil"]
   ["stack.yaml" "haskell"] ["cabal.project" "haskell"] ["package.yaml" "haskell"]
   ["tsconfig.json" "typescript"]
   ["pyproject.toml" "python"] ["setup.py" "python"] ["setup.cfg" "python"]
   ["requirements.txt" "python"] ["Pipfile" "python"]])

(def (detect-language)
  (cond
   ((find (lambda (m) (file-exists? (car m))) MARKERS) => (lambda (m) (find-language (cadr m))))
   ((file-matching? (has-ext? ".cabal")) haskell)
   (else
    (let ((counted (count-by-language)))
      (cond
       ((pair? counted) (car counted))
       ((file-exists? "package.json") javascript)
       (else generic))))))

;; Languages of the files in . and a few usual source directories, most first.
(def (count-by-language)
  (let ((counts (make-hash-table)))
    (for-each
     (lambda (dir)
       (when (file-directory? dir)
         (for-each (lambda (f)
                     (let ((l (language-for-path f)))
                       (when l (hash-update! counts l (lambda (k) (+ k 1)) 0))))
                   (directory-files dir))))
     '("." "src" "lib" "app" "test" "tests"))
    (map car (sort (hash->list counts) (lambda (a b) (> (cdr a) (cdr b)))))))

;; ---------------------------------------------------------------------------
;; Eval and shell

;; eval-command : string string -> args or #f
(def (eval-command code tmpdir)
  (let ((ev (language-evaluator (current-lang))))
    (and ev (ev code tmpdir))))

(def (shell-commands)
  (append BASE-SHELL-COMMANDS (language-shell (current-lang))))
