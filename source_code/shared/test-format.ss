;; File: test-format.ss
(import :std/test
        "./format")

(export main)

(def format-test
  (test-suite "shared format tests"
    (test-case "negative numbers"
      (check (format-decimal -0.5 2) => "-0.50")
      (check (format-decimal -0.05 2) => "-0.05")
      (check (format-decimal -12.346 2) => "-12.35")
      (check (format-decimal -0.0001 2) => "0.00"))
    (test-case "leading zeros"
      (check (format-decimal 0.74 2) => "0.74")
      (check (format-decimal 0.05 2) => "0.05")
      (check (format-decimal 0.0 2) => "0.00")
      (check (format-decimal 5 2) => "5.00"))
    (test-case "large numbers"
      (check (format-decimal 1000000.5 1) => "1000000.5"))
    (test-case "percent"
      (check (percent 0.74 1) => "74.0%")
      (check (percent 0.1234 2) => "12.34%"))
    (test-case "with-commas"
      (check (with-commas 1000) => "1,000")
      (check (with-commas 1000000) => "1,000,000")
      (check (with-commas 42) => "42"))))

(def (main . args)
  (run-tests! format-test)
  (test-report-summary!)
  (exit (case (test-result) ((OK) 0) (else 1))))
