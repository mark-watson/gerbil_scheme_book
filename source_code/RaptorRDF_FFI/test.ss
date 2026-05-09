;; Test suite for ffi.ss: validates N-Triples output from Raptor2 FFI

(import "ffi" :gerbil/gambit)
(export main)

(define (assert-equal expected actual label)
  (if (equal? expected actual)
      (begin (display "PASS ") (display label) (newline) #t)
      (begin
        (display "FAIL ") (display label) (newline)
        (display "Expected:\n") (display expected) (newline)
        (display "Actual:\n") (display actual) (newline)
        (exit 1))))

(define (main . args)
  (let* ((ttl-file "sample.ttl")
         (ttl-content "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .\n")
         (expected-nt "<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n"))

    ;; Prepare sample Turtle file
    (call-with-output-file ttl-file
      (lambda (p) (display ttl-content p)))

    ;; 1. Parse with explicit syntax
    (let ((nt1 (raptor-parse-file->ntriples ttl-file "turtle")))
      (assert-equal expected-nt nt1 "turtle -> ntriples"))

    ;; 2. Parse with syntax guessing
    (let ((nt2 (raptor-parse-file->ntriples ttl-file "guess")))
      (assert-equal expected-nt nt2 "guess -> ntriples"))

    ;; 3. High-level API with default syntax
    (let ((nt3 (parse-rdf-file ttl-file)))
      (assert-equal expected-nt nt3 "parse-rdf-file (default syntax)"))

    ;; 4. Structured triples API
    (let ((triples (parse-rdf-file->triples ttl-file)))
      (assert-equal 1 (length triples) "parse-rdf-file->triples returns 1 triple")
      (assert-equal '("<http://example.org/s>"
                      "<http://example.org/p>"
                      "<http://example.org/o>")
                    (car triples)
                    "parse-rdf-file->triples structure"))

    ;; 5. Error handling: nonexistent file returns empty string
    (let ((bad (raptor-parse-file->ntriples "no-such-file.ttl" "turtle")))
      (assert-equal "" bad "nonexistent file returns empty string"))

    ;; Clean up
    (when (file-exists? ttl-file)
      (delete-file ttl-file))

    (display "All tests passed.\n")))
