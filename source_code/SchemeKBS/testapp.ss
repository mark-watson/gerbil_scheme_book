(import :kbtm/main)

(export main)

;; minimal JSON writer for our specific output
(define (json-escape s)
  (list->string
   (apply append
          (map (lambda (ch)
                 (cond
                  ((char=? ch #\") '(#\\ #\"))
                  ((char=? ch #\\) '(#\\ #\\))
                  ((char=? ch #\newline) '(#\\ #\n))
                  (else (list ch))))
               (string->list s)))))

(define (write-json-string s)
  (display "\"")
  (display (json-escape s))
  (display "\""))

(define (write-json-string-list lst)
  (display "[")
  (let loop ((xs lst) (first #t))
    (if (pair? xs)
        (begin
          (if (not first) (display ","))
          (write-json-string (car xs))
          (loop (cdr xs) #f))))
  (display "]"))

(define (write-json-categories cats)
  ;; cats: list of ((name score) ...)
  (display "[")
  (let loop ((xs cats) (first #t))
    (if (pair? xs)
        (let* ((pair (car xs))
               (name (car pair))
               (score (cadr pair)))
          (if (not first) (display ","))
          (display "[")
          (write-json-string name)
          (display ",")
          (display score)
          (display "]")
          (loop (cdr xs) #f))))
  (display "]"))

(define (json-write ret)
  ;; ret is a table with fixed keys
  (display "{")
  (display "\"words\":")
  (write-json-string-list (table-ref ret "words" '()))
  (display ",\"tags\":")
  (write-json-string-list (table-ref ret "tags" '()))
  (display ",\"key-phrases\":")
  (write-json-string-list (table-ref ret "key-phrases" '()))
  (display ",\"categories\":")
  (write-json-categories (table-ref ret "categories" '()))
  (display "}") )

(define (print-help)
  (display "KBtextmaster (native) command line arguments:")
  (newline)
  (display "   -h              -- to print help message")
  (newline)
  (display "   -i <file name>  -- to define the input file name")
  (newline)
  (display "   -o <file name>  -- to specify the output file name")
  (newline))


(define (main . argv)
  (let* ((args (command-line))
         (in-file (member "-i" args))
         (out-file (member "-o" args))
         (ret (make-table)))
    (when (member "-h" args)
      (print-help))
    (set! in-file (and in-file (cadr in-file)))
    (set! out-file (and out-file (cadr out-file)))
    (if (and in-file out-file)
        (let ((resp (process-file in-file)))
          (with-output-to-file
              (list path: out-file create: #t)
            (lambda ()
              (table-set! ret "words" (vector->list (car resp)))
              (table-set! ret "tags" (vector->list (cadr resp)))
              (table-set! ret "key-phrases" (caddr resp))
              (table-set! ret "categories" (cadddr resp))
              ;; TBD: implement summary words, proper name list, and place name list

              (json-write ret))))
        (print-help))
    0))

;;(write "In testapp....") 
;;(newline)

;;(write (process-file "data/testdata/climate_g8.txt")  )
;;(newline)
