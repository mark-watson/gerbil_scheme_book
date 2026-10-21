;;; Tests for the delimiter scanner (parens.ss) and the syntax-error
;;; classifier (checker.ss).   Run: gxi test/parens-test.ss

(import :std/test
        "../parens"
        "../checker")

(export main)

(def (kinds text)
  (map paren-problem-kind (paren-report-problems (scan-parens text))))

(def (first-problem text)
  (car (paren-report-problems (scan-parens text))))

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

    (test-case "syntax-trouble classification"
      (check (car (syntax-trouble "--- Syntax Error at read-module: error reading module\n... detail: \"Incomplete form, EOF reached\\n\"" []))
             => 'reader)
      (check (car (syntax-trouble "--- Syntax Error at (expand foo): Reference to unbound identifier" []))
             => 'syntax)
      (check (syntax-trouble "test failed: expected 3 got 4" []) => #f))))

(def (main . args)
  (run-tests! parens-test)
  (test-report-summary!)
  (exit (case (test-result) ((OK) 0) (else 1))))
