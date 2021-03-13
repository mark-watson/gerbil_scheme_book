;;;;; Proper names

;;; Human names:

(define *first-name-hash* (make-table size: 10000))
(define *last-name-hash*  (make-table size: 140000))

(define *name-prefix-list*
  '("Mr" "Mrs" "Ms" "Gen" "General" "Maj" "Major" "Doctor" "Vice" "President" 
	"Lt" "Premier" "Senator" "Congressman" "Prince" "King" "Representative"
	"Sen" "St" "Dr"))

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
(define (find-human-names word-vector exclusion-list)
  (define (score result-list)
	(- 1.0 (* 0.2 (- 4 (length result-list)))))
  (let ((tags (parts-of-speech word-vector))
		(ret '()) (ret2 '()) (x '())
		(len (vector-length word-vector))
		(word #f))
    (for (i (in-range len))
	  (set! word (vector-ref word-vector i))
	  ;; process 4 word names:
	  (if (< i (- len 3))
		  ;; case #1: single element from '*name-prefix-list*'
		  (if (and
			   (not-in-list-find-names-helper ret i (+ i 4))
			   (not-in-list-find-names-helper exclusion-list i (+ i 4))
			   (member word *name-prefix-list*)
			   (equal? "." (vector-ref word-vector (+ i 1)))
			   (gethash (vector-ref word-vector (+ i 2)) *first-name-hash*)
			   (gethash (vector-ref word-vector (+ i 3)) *last-name-hash*))
			  (if (and
				   (string-prefix? "NN" (vector-ref tags (+ i 2)))
				   (string-prefix? "NN" (vector-ref tags (+ i 3))))
				  (set! ret (cons (list i (+ i 4)) ret))))
		  ;; case #1: two elements from '*name-prefix-list*'
		  (if (and
			   (not-in-list-find-names-helper ret i (+ i 4))
			   (not-in-list-find-names-helper exclusion-list i (+ i 4))
			   (member word *name-prefix-list*)
			   (member (vector-ref word-vector (+ i 1)) *name-prefix-list*)
			   (gethash (vector-ref word-vector (+ i 2)) *first-name-hash*)
			   (gethash (vector-ref word-vector (+ i 3)) *last-name-hash*))
			  (if (and
				   (string-prefix? "NN" (vector-ref tags (+ i 2)))
				   (string-prefix? "NN" (vector-ref tags (+ i 3))))
				  (set! ret (cons (list i (+ i 4)) ret)))))
	  ;; process 3 word names:
	  (if (< i (- len 2))
		  (if (and
			   (not-in-list-find-names-helper ret i (+ i 3))
			   (not-in-list-find-names-helper exclusion-list i (+ i 3)))
			  (if (or
				   (and
					(member word *name-prefix-list*)
					(gethash (vector-ref word-vector (+ i 1)) *first-name-hash*)
					(gethash (vector-ref word-vector (+ i 2)) *last-name-hash*)
					(string-prefix? "NN" (vector-ref tags (+ i 1)))
					(string-prefix? "NN" (vector-ref tags (+ i 2))))
				   (and
					(member word *name-prefix-list*)
					(member (vector-ref word-vector (+ i 1)) *name-prefix-list*)
					(gethash (vector-ref word-vector (+ i 2)) *last-name-hash*)
					(string-starts-with (vector-ref tags (+ i 1)) "NN")
					(string-starts-with (vector-ref tags (+ i 2)) "NN"))
				   (and
					(member word *name-prefix-list*)
					(equal? "." (vector-ref word-vector (+ i 1)))
					(gethash (vector-ref word-vector (+ i 2)) *last-name-hash*)
					(string-starts-with (vector-ref tags (+ i 2)) "NN"))
				   (and
					(gethash word *first-name-hash*)
					(gethash (vector-ref word-vector (+ i 1)) *first-name-hash*)
					(gethash (vector-ref word-vector (+ i 2)) *last-name-hash*)
					(string-starts-with (vector-ref tags i) "NN")
					(string-starts-with (vector-ref tags (+ i 1)) "NN")
					(string-starts-with (vector-ref tags (+ i 2)) "NN")))
				  (set! ret (cons (list i (+ i 3)) ret)))))
	  ;; process 2 word names:
	  (if (< i (- len 1))
		  (if (and
			   (not-in-list-find-names-helper ret i (+ i 2))
			   (not-in-list-find-names-helper exclusion-list i (+ i 2)))
			  (if (or
				   (and
					(member word '("Mr" "Mrs" "Ms" "Doctor" "President" "Premier"))
					(string-starts-with (vector-ref tags (+ i 1)) "NN")
					(gethash (vector-ref word-vector (+ i 1)) *last-name-hash*))
				   (and
					(gethash word *first-name-hash*)
					(gethash (vector-ref word-vector (+ i 1)) *last-name-hash*)
					(string-starts-with (vector-ref tags i) "NN")
					(string-starts-with (vector-ref tags (+ i 1)) "NN")))
				  (set! ret (cons (list i (+ i 2)) ret)))))
	  ;; 1 word names:
	  (if (gethash word *last-name-hash*)
		  (if (and
			   (string-starts-with (vector-ref tags i) "NN")
			   (not-in-list-find-names-helper ret i (+ i 1))
			   (not-in-list-find-names-helper exclusion-list i (+ i 1)))
			  (set! ret (cons (list i (+ i 1)) ret)))))
	;; TBD: calculate importance rating based on number of occurences of name in text:
	(set! ret2
	      (map (lambda (index-pair)
		     (subvector  word-vector (car index-pair) (cadr index-pair)))
		   ret))
    ret2))

;; test: (find-human-names '#("President" "Bush" "went" "to" "San" "Diego" "to" "meet" "Ms" "." "Jones" "and" "Gen" "." "Pervez" "Musharraf" ".") '())

(define (not-in-list-find-names-helper a-list start end)
  (let ((rval #t))
    (do ((x a-list (cdr x)))
	((or
	  (null? x)
	  (let ()
	    (if (or
		 (and
		  (>= start (caar x))
		  (<= start (cadar x)))
		 (and
		  (>= end (caar x))
		  (<= end (cadar x))))
		(set! rval #f))
	    (not rval)))))
    rval))



;;; Common data loading utility for all types of name data:

(define (load-name-data)
  (process-one-word-per-line 
   "data/human_names/names.male"
   (lambda (word) (table-set! *first-name-hash* word #t)))
  (process-one-word-per-line 
   "data/human_names/names.female"
   (lambda (word) (table-set! *first-name-hash* word #t)))
  (process-one-word-per-line 
   "data/human_names/names.last"
   (lambda (word) (table-set! *last-name-hash* word #t))))

