;;;;;;;;;;;;;;;;;;;; 3 groups of utilities in this file:
;;
;;;;;;;;;;;;;; file utilities
;;;;;;;;;;;;;; string functions
;;;;;;;;;;;;;; sort

(export file->string process-one-word-per-line
        string->lower-case-string string-downcase
        substring? string-suffix? string-prefix? %string-prefix-length
        contraction-hash noise-word? no-noise
        split-seq-if words-from-string
        sorted? merge merge! sort! sort)

(import :std/iter) ;; loop utilities
(import :std/misc/ports)

;; Load stop words at compile-time from included data
(include "data/stop-words.ss")

;;;;;;;;; macros:

;;;;;; file utilities:
(define (file->string file-path)
  (read-file-string file-path)) ;; just use Gerbil ports library

(define (process-one-word-per-line file-path func)
  (for (line (read-file-lines file-path))
       (func line)))

;; test:   (process-one-word-per-line "data/human_names/names.male" display)

(define (string->lower-case-string s)
  (string-downcase s))

;; Return a new string made of characters of the
;; original string in the lower case
(define (string-downcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (+ 1 i)))
      ((>= i (string-length str)) target-str)
    (string-set! target-str i (char-downcase (string-ref str i)))))



;; string functions:

(define substring?
  (lambda (s1 s2)
    (let* ((s1-len (string-length s1))
           (s2-len (string-length s2))
           (n-give-up (+ 1 (- s2-len s1-len))))
      (let loop ((i 0))
        (if (< i n-give-up)
            (let loop2 ((j 0) (k i))
              (if (< j s1-len)
                  (if (char=? (string-ref s1 j) (string-ref s2 k))
                      (loop2 (+ j 1) (+ k 1))
                      (loop (+ i 1)))
                  i))
            #f)))))


(define (string-suffix? pattern str)
  (let loop ((i (- (string-length pattern) 1)) (j (- (string-length str) 1)))
    (cond
     ((negative? i) #t)
     ((negative? j) #f)
     ((char=? (string-ref pattern i) (string-ref str j))
      (loop (- i 1) (- j 1)))
     (else #f))))


(define (string-prefix? pattern str)
  (let ((len (string-length pattern)))
    (and
     (>= (string-length str) len)
     (equal? len (%string-prefix-length pattern 0 len str 0 100)))))

(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char=? (string-ref s1 i)
			       (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))


;; STRING-PARSE

(define contraction-hash (make-table))

(table-set! contraction-hash "I'm" '("am" "I"))
(table-set! contraction-hash "can't" '("not" "can"))
(table-set! contraction-hash "Can't" '("not" "Can"))
(table-set! contraction-hash "that's" '("is" "that"))
(table-set! contraction-hash "That's" '("is" "That"))
(table-set! contraction-hash "Jan" '("January"))
(table-set! contraction-hash "Feb" '("February"))
(table-set! contraction-hash "Mar" '("March"))
(table-set! contraction-hash "Apr" '("April"))
(table-set! contraction-hash "Jun" '("June"))
(table-set! contraction-hash "Jul" '("July"))
(table-set! contraction-hash "Aug" '("August"))
(table-set! contraction-hash "Sept" '("September"))
(table-set! contraction-hash "Sep" '("September"))
(table-set! contraction-hash "Oct" '("October"))
(table-set! contraction-hash "Nov" '("November"))
(table-set! contraction-hash "Dec" '("December"))

;; Noise words:

;;(define noise-word-hash (make-table))

;;(process-one-word-per-line
;; "data/stopwords.txt"
;; (lambda (word) (table-set! noise-word-hash  (string-downcase word) #t)))

(define (noise-word? word) (table-ref noise-word-hash (string-downcase word) #f))

(define (no-noise words)
  (let ((ret '())
	(num (vector-length words))
	(word #f))
    (for (n (in-range num))
	 (set! word (vector-ref words n))
	 (if (not (noise-word? word)) (set! ret (cons word ret))))
    (list->vector (reverse ret))))


;; utility:

(define (split-seq-if test? str)
  (let ((ret '())
        (temp '())
        (len (string-length str)))
    (do ((i 0 (+ i 1)))
        ((eq? i len))
      (let ((ch (string-ref str i)))
        (if (test? ch)
            (begin
              (if (> (length temp) 0)
                  (set! ret (cons (list->string (reverse temp)) ret)))
              (set! temp '()))
            (set! temp (cons ch temp)))))
    (if (> (length temp) 0)
        (set! ret (cons (list->string (reverse temp)) ret)))
    (reverse ret)))

;; test: (split-seq-if (lambda (x) (member x '(#\space  #\: #\,))) "The cat can't run, slowly in Oct. . . . . The U.S.A. economy is strong. Is it OK?")

;;      this now returns a vector:  make this more efficient!!!  TBD!!

(define (words-from-string str)
  (list->vector (split-seq-if (lambda (ch) (char=? ch #\space)) str))) ;; simple whitespace split


;; test:  (words-from-string "The cat can't run, slowly in Oct. . . . . The U.S.A. economy is strong. Is it OK?")


;; sort:

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;      (not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sorted? seq less?)
  (cond
   ((null? seq)
    #t)
   ((vector? seq)
    (let ((n (vector-length seq)))
      (if (<= n 1)
          #t
          (do ((i 1 (+ i 1)))
              ((or (= i n)
                   (less? (vector-ref seq (- i 1))
                          (vector-ref seq i)))
               (= i n)) )) ))
   (else
    (let loop ((last (car seq)) (next (cdr seq)))
      (or (null? next)
          (and (not (less? (car next) last))
               (loop (car next) (cdr next)) )) )) ))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
  (cond
   ((null? a) b)
   ((null? b) a)
   (else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
           ;; The loop handles the merging of non-empty lists.  It has
           ;; been written this way to save testing and car/cdring.
           (if (less? y x)
               (if (null? b)
                   (cons y (cons x a))
                   (cons y (loop x a (car b) (cdr b)) ))
               ;; x <= y
               (if (null? a)
                   (cons x (cons y b))
                   (cons x (loop (car a) (cdr a) y b)) )) )) ))


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
  (define (loop r a b)
    (if (less? (car b) (car a))
        (begin
          (set-cdr! r b)
          (if (null? (cdr b))
              (set-cdr! b a)
              (loop b a (cdr b)) ))
        ;; (car a) <= (car b)
        (begin
          (set-cdr! r a)
          (if (null? (cdr a))
              (set-cdr! a b)
              (loop a (cdr a) b)) )) )
  (cond
   ((null? a) b)
   ((null? b) a)
   ((less? (car b) (car a))
    (if (null? (cdr b))
        (set-cdr! b a)
        (loop b a (cdr b)))
    b)
   (else ; (car a) <= (car b)
    (if (null? (cdr a))
        (set-cdr! a b)
        (loop a (cdr a) b))
    a)))



;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq less?)
  (define (step n)
    (cond
     ((> n 2)
      (let* ((j (quotient n 2))
             (a (step j))
             (k (- n j))
             (b (step k)))
        (merge! a b less?)))
     ((= n 2)
      (let ((x (car seq))
            (y (cadr seq))
            (p seq))
        (set! seq (cddr seq))
        (if (less? y x) (begin
                          (set-car! p y)
                          (set-car! (cdr p) x)))
        (set-cdr! (cdr p) '())
        p))
     ((= n 1)
      (let ((p seq))
        (set! seq (cdr seq))
        (set-cdr! p '())
        p))
     (else
      '()) ))
  (if (vector? seq)
      (let ((n (vector-length seq))
            (vector seq))                     ; save original vector
        (set! seq (vector->list seq))       ; convert to list
        (do ((p (step n) (cdr p))           ; sort list destructively
             (i 0 (+ i 1)))                         ; and store elements back
            ((null? p) vector)              ; in original vector
          (vector-set! vector i (car p)) ))
      ;; otherwise, assume it is a list
      (step (length seq)) ))


;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence
(define (sort seq less?)
  (if (vector? seq)
      (list->vector (sort! (vector->list seq) less?))
      (sort! (append seq '()) less?)))
