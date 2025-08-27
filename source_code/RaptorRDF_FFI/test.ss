;; Simple test for ffi.ss: validates N-Triples output

(import "ffi")

(define (write-file path content)
  (let ((p (open-output-file path)))
    (display content p)
    (close-output-port p)))




(export main)
(import "ffi" :gerbil/gambit)



(define (read-file path)
  (let ((p (open-input-file path)))
    (let loop ((chunks '()))
      (let ((c (read-char p)))
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse chunks)))
            (loop (cons c chunks)))))))

(define (assert-equal expected actual label)
  (if (equal? expected actual)
      (begin (display "PASS ") (display label) (newline) #t)
      (begin
        (display "FAIL ") (display label) (newline)
        (display "Expected:
") (display expected) (newline)
        (display "Actual:
") (display actual) (newline)
        (exit 1))))

(define (main . args)
  (let* ((ttl-file "sample.ttl")
         (ttl-content "@prefix ex: <http://example.org/> .
ex:s ex:p ex:o .
")
         (expected-nt "<http://example.org/s> <http://example.org/p> <http://example.org/o> .
"))
    ;; Prepare sample Turtle file
    (write-file ttl-file ttl-content)

    ;; Exercise FFI with explicit syntax
    (let ((nt1 (raptor-parse-file->ntriples ttl-file "turtle")))
      (assert-equal expected-nt nt1 "turtle -> ntriples"))

    ;; Exercise FFI with syntax guessing
    (let ((nt2 (raptor-parse-file->ntriples ttl-file "guess")))
      (assert-equal expected-nt nt2 "guess -> ntriples"))

    ;; Clean up
    (when (file-exists? ttl-file)
      (delete-file ttl-file))

    (display "All tests passed.
")))

