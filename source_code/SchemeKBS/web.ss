;;; File: "simple-http-server.ss"

;; note: Marc Feeley posted this in response to a question I asked on the Scheme newsgroup

;; to run:     gsi -e "(load \"web\")"

(load "ll")

(define http-server-address "*:8000") ;; "*:80" is standard

(define (http-server-start)
  (let ((accept-port
         (open-tcp-server (list server-address: http-server-address
                                eol-encoding: 'cr-lf))))
    (let loop ()
      (let ((connection (read accept-port)))
        (if (not (eof-object? connection))
            (begin
              (http-serve connection)
              (loop)))))))

(define (http-serve connection)
  (let ((request (read-line connection)))
    (if (string? request)
        (let* ((r
                (call-with-input-string
                 request
                 (lambda (p)
                   (read-all p (lambda (p)
                                 (read-line p #\space))))))
               (method
                (car r)))
          (cond ((string-ci=? method "GET")
                 (http-get connection (cadr r)))
                (else
                 (println "unhandled request: " request)))))
    (close-port connection)))

(define (http-get connection document)
  (print (list "document:" document))
  (print port: connection
         "HTTP/1.0 200 OK\n"
         "Content-Type: text/html; charset=ISO-8859-1\n"
         "Connection: close\n"
         "\n"
         (<html>
          (<body>
           (<h3> "Enter text that contains peoples names")
           (<form>
            "<input type=\"text\" name=\"text\" size=\"80\" /><br/>"
            "<input type=\"submit\" />")
           "<br/>\n"
           (if (and
                (> (string-length document) 0)
                (substring? "=" document))
               (<pre>
		"Names in text:\n"
		(map (lambda (str) (list str "\n"))
                     (find-human-names (list->vector (string-tokenize document)) '()))))))))

;; Note: when you define a function using a form like:
;;     (define (function-name . body) ..
;; then the argument following the period is bound to a list of any arguments
;; passed to the function.

(define (<html> . body) (list "<html>" body "</html>"))
(define (<body> . body) (list "<body>" body "</body>"))
(define (<h3> . body) (list "<h3>" body "</h3>"))
(define (<pre> . body) (list "<pre>" body "</pre>"))
(define (<form> . body) (list "<form>" body "</form>"))

(http-server-start)

