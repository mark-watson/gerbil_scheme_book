;;;;; Place names

;; TBD: implement this!

(define (string-starts-with s1 s2) (string-prefix? s2 s1))
(define (gethash key hash) (table-ref hash key #f))

(define (string-tokenize str)
  (let ((ret '())
        (temp '())
        (len (string-length str)))
    (for (i (in-range len))
      (let ((ch (string-ref str i)))
        (if (member ch '(#\space #\newline  #\: #\, #\. #\! #\; #\+ #\=))
            (begin
              (if (> (length temp) 0)
                  (set! ret (cons (list->string (reverse temp)) ret)))
              (if (and (not (equal? ch #\ )) (not (equal? ch #\+))) (set! ret (cons (string ch) ret)))
              (set! temp '()))
            (set! temp (cons ch temp)))))
    (if (> (length temp) 0)
        (set! ret (cons (list->string (reverse temp)) ret)))
    (reverse ret)))

;; return a list of sublists, each sublist looks like:
;;    (("John" "Smith") (11 12) 0.75) ; last number is an importance rating
(define (find-place-names word-vector)
  '())

(define (load-place-data)  ;; TBD: convert this code to read place data files; create place data files !!!
  (process-one-word-per-line 
   "data/human_names/names.male"
   (lambda (word) (table-set! *first-name-hash* word #t)))
  (process-one-word-per-line 
   "data/human_names/names.female"
   (lambda (word) (table-set! *first-name-hash* word #t)))
  (process-one-word-per-line 
   "data/human_names/names.last"
   (lambda (word) (table-set! *last-name-hash* word #t))))

