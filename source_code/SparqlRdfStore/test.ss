(import "rdfwrap"
	:std/srfi/13)
(export main)

(define default-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }")

(define (usage name)
  (display "Usage: ") (display name) (display " [data-file [query]]\n")
  (display "  data-file: RDF file (ttl/n3/nt). Default: mini.nt\n")
  (display "  query    : SPARQL SELECT query or @file to read from file. Default: ")
  (display default-query) (newline))

(define (main . args)
  (let* ((prog DEMO_rdfwrap") ; shuld use (car (commandline)) but not available on Linux
         (path (if (pair? args) (car args) "mini.nt"))
         (rawq (if (and (pair? args) (pair? (cdr args))) (cadr args) default-query))
         (query (if (string-prefix? "@" rawq)
                     (let ((f (substring rawq 1 (string-length rawq))))
                       (call-with-input-file f
                         (lambda (p)
                           (let loop ((acc '()))
                             (let ((ch (read-char p)))
                               (if (eof-object? ch)
                                   (list->string (reverse acc))
                                   (loop (cons ch acc))))))))
                     rawq)))
    (when (or (string=? path "-h") (string=? path "--help"))
      (usage prog)
      (exit 0))
    (unless (zero? (rdf-init path)) (error "rdf-init failed" path))
    (let ((out (rdf-query query)))
      (when out (display out)))
    (rdf-free)
    (exit 0)))
