(import :std/text/json)

(define (print-help)
  (display "KBtextmaster (native) command line arguments:")
  (newline)
  (display "   -h              -- to print help message")
  (newline)
  (display "   -i <file name>  -- to define the input file name")
  (newline)
  (display "   -o <file name>  -- to specify the output file name")
  (newline))


(let* ((args (command-line))
       (in-file (member "-i" args))
       (out-file (member "-o" args))
       (ret (make-table)))
  (if (member "-h" args)
      (print-help))
  (if in-file
      (set! in-file (cadr in-file))
      (set! in-file #f))
  (if out-file
      (set! out-file (cadr out-file))
      (set! out-file #f))
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

             (json-write ret)
            )))
      (print-help)))

;;(write "In testapp....") 
;;(newline)

;;(write (process-file "data/testdata/climate_g8.txt")  )
;;(newline)
