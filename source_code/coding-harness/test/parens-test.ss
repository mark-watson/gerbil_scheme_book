;;; Tests for the delimiter scanner (parens.ss), the language profiles
;;; (lang.ss) and the syntax-error classifier (checker.ss).
;;; Run: gxi test/parens-test.ss

(import :std/test
        :std/sugar
        "../parens"
        "../lang"
        "../checker")

(export main)

(def (kinds text)
  (map paren-problem-kind (paren-report-problems (scan-parens text))))

(def (first-problem text)
  (car (paren-report-problems (scan-parens text))))

(def (kinds-in lexicon text)
  (map paren-problem-kind (paren-report-problems (scan-delimiters text lexicon))))

(def (first-in lexicon text)
  (car (paren-report-problems (scan-delimiters text lexicon))))

(def (with-language name thunk)
  (let ((saved (current-lang)))
    (select-language! (find-language name))
    (try (thunk) (finally (select-language! saved)))))

(def parens-test
  (test-suite "delimiter scanner"
    (test-case "balanced code with tricky tokens"
      (check (kinds "(def s \"a ( b\")") => [])
      (check (kinds "(def c #\\( ) (def d #\\))") => [])
      (check (kinds "(def q #\\\") (def sc #\\;)") => [])
      (check (kinds ";; (((\n(def x 1)") => [])
      (check (kinds "#| ( #| nested ) |# ] |#\n(def x 1)") => [])
      (check (kinds "(def v #(1 2)) (def l [1 {a b}])") => [])
      (check (kinds "(def |odd (sym| 1)") => [])
      (check (kinds "(def h #<<END\n( [ {\nEND\n  )") => [])
      (check (kinds "(def e #\\space) (def x #\\x41)") => []))

    (test-case "missing close paren"
      (let* ((text "(def (foo x)\n  (let ((y 1))\n    (+ x y))\n\n(def (bar) 2)\n")
             (report (scan-parens text))
             (p (car (paren-report-problems report))))
        (check (paren-problem-kind p) => 'unclosed)
        (check (paren-problem-line p) => 1)
        (check (paren-problem-col p) => 1)
        ;; the column-1 heuristic points at the line where the close is missing
        (check (length (paren-report-hints report)) => 1)))

    (test-case "extra close paren"
      (let ((p (first-problem "(def (foo x) (+ x 1)))\n")))
        (check (paren-problem-kind p) => 'unexpected-close)
        (check (paren-problem-line p) => 1)
        (check (paren-problem-col p) => 22)))

    (test-case "mismatched bracket"
      (let ((p (first-problem "(def (f x)\n  (let ((y [1 2)))\n    y))\n")))
        (check (paren-problem-kind p) => 'mismatch)
        (check (paren-problem-line p) => 2)
        (check (paren-problem-col p) => 16)))

    (test-case "unterminated string and comment"
      (check (car (kinds "(def s \"oops)\n(def x 1)\n")) => 'unterminated-string)
      (check (car (kinds "#| never closed\n(def x 1)\n")) => 'unterminated-comment))

    (test-case "top-level forms"
      (let ((forms (paren-report-forms (scan-parens "(import :std/sugar)\n\n(def (f)\n  1)\n"))))
        (check (length forms) => 2)
        (check (paren-form-line (cadr forms)) => 3)
        (check (paren-form-end-line (cadr forms)) => 4)
        (check (paren-form-head (cadr forms)) => "(def (f)"))
      (check (top-level-form-strings "(import \"x\") (f 1)") => ["(import \"x\")" "(f 1)"]))

    (test-case "syntax-trouble classification (Gerbil)"
      (with-language "gerbil"
        (lambda ()
          (check (car (syntax-trouble "--- Syntax Error at read-module: error reading module\n... detail: \"Incomplete form, EOF reached\\n\"" []))
                 => 'reader)
          (check (car (syntax-trouble "--- Syntax Error at (expand foo): Reference to unbound identifier" []))
                 => 'syntax)
          (check (syntax-trouble "test failed: expected 3 got 4" []) => #f))))

    (test-case "syntax-trouble classification (other languages)"
      (with-language "python"
        (lambda ()
          (check (car (syntax-trouble "x.py:3:5: SyntaxError: '(' was never closed" [])) => 'reader)
          (check (car (syntax-trouble "x.py:3:5: SyntaxError: invalid syntax" [])) => 'syntax)
          (check (car (syntax-trouble "  File \"x.py\", line 2\nIndentationError: unexpected indent" [])) => 'syntax)
          (check (syntax-trouble "FAILED tests/test_x.py::test_add - assert 3 == 4" []) => #f)))
      (with-language "haskell"
        (lambda ()
          (check (car (syntax-trouble "B.hs:3:1: error: [GHC-58481]\n    parse error (possibly incorrect indentation or mismatched brackets)" []))
                 => 'reader)
          (check (car (syntax-trouble "B.hs:3:1: error: parse error on input '='" [])) => 'syntax)
          (check (syntax-trouble "B.hs:3:1: error: Variable not in scope: foo" []) => #f)))
      (with-language "typescript"
        (lambda ()
          (check (car (syntax-trouble "a.ts(3,5): error TS1128: Declaration or statement expected." [])) => 'syntax)
          (check (syntax-trouble "a.ts(3,5): error TS2322: Type 'string' is not assignable to type 'number'." []) => #f))))

    (test-case "python lexicon"
      (check (kinds-in 'python "s = \"\"\"a ( b\n]\"\"\"  # ) comment\nx = [1, '(', \")\"]\n") => [])
      (check (kinds-in 'python "x = 'abc\ny = 2\n") => '(unterminated-string))
      (let ((p (first-in 'python "def f(x):\n    return g(x\n\ndef h():\n    pass\n")))
        (check (paren-problem-kind p) => 'unclosed)
        (check (paren-problem-line p) => 2))
      ;; the column-1 hint points at the next top-level def
      (check (length (paren-report-hints (scan-delimiters "def f(x):\n    return g(x\n\ndef h():\n    pass\n" 'python)))
             => 1))

    (test-case "haskell lexicon"
      (check (kinds-in 'haskell "f :: Int -> Int\nf x' = x' + ord '(' -- )\n{- ( {- nested ) -} -}\ng = (x --> y)\n") => [])
      (check (kinds-in 'haskell "f = [ '\\n', ']' ]\n") => [])
      (check (kinds-in 'haskell "f x = (x + 1\n") => '(unclosed)))

    (test-case "c-like lexicon"
      (check (kinds-in 'c-like "const r = /[(]/g; const t = `a ( ${b}`;\nlet z = a / (b) / c; // )\n/* ( */\n") => [])
      (check (kinds-in 'c-like "const s = \"a ) b\"; const q = 'x ] y';\n") => [])
      (let ((p (first-in 'c-like "function f() {\n  if (x) {\n    return 1;\n}\nexport const y = 2;\n")))
        (check (paren-problem-kind p) => 'unclosed)
        (check (paren-problem-line p) => 1))
      (check (kinds-in 'c-like "f(a, b];\n") => '(mismatch)))

    (test-case "languages by path"
      (check (language-name (language-for-path "src/app.py")) => "python")
      (check (language-name (language-for-path "Main.hs")) => "haskell")
      (check (language-name (language-for-path "index.ts")) => "typescript")
      (check (language-name (language-for-path "util.mjs")) => "javascript")
      (check (language-name (language-for-path "harness.ss")) => "gerbil")
      (check (language-for-path "README.md") => #f)
      (check (lexicon-for-path "App.tsx") => #f)
      (check (lexicon-for-path "app.ts") => 'c-like))

    (test-case "error locations"
      ;; this test file exists, so locations pointing into it are kept
      (check (error-locations "test/parens-test.ss:3:5: error") => '(("test/parens-test.ss" 3 5)))
      (check (error-locations "File \"test/parens-test.ss\", line 7") => '(("test/parens-test.ss" 7 1)))
      (check (error-locations "\"test/parens-test.ss\"@12.4-12.9") => '(("test/parens-test.ss" 12 4)))
      (check (error-locations "nofile.py:3:5: error") => []))))

(def (main . args)
  (run-tests! parens-test)
  (test-report-summary!)
  (exit (case (test-result) ((OK) 0) (else 1))))
