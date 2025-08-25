;;; Find key phrases in text


(define (key-phrases input-file-path)
  (let ((words (words-from-string (file->string input-file-path) '("\"" "." "-" "--")))
        (1-word (make-table size: 1000))
        (2-words (make-table size: 4000))
        (3-words (make-table size: 10000))
        ;; data for 3 return lists:
        (1-r '()) (2-r '()) (3-r '())
        (numwords 0))
    (set! numwords (vector-length words))
    (for (n (in-sequence (- numwords 2)))
         (let ((1-s (vector-ref words n))
               (2-s (string-append (vector-ref words n) " " (vector-ref words (+ n 1))))
               (3-s (string-append (vector-ref words n) " " (vector-ref words (+ n 1)) " " (vector-ref words (+ n 2)))))
           (if (not (noise-word? (vector-ref words n)))
               (table-set! 1-word 1-s (+ 1 (table-ref 1-word 1-s 0))))
           (if (and
                (not (noise-word? (vector-ref words n)))
                (not (noise-word? (vector-ref words (+ 1 n)))))
               (table-set! 2-words 2-s (+ 1 (table-ref 2-words 2-s 0))))
           (if (and
                (not (noise-word? (vector-ref words n)))
                (not (noise-word? (vector-ref words (+ 1 n))))
                (not (noise-word? (vector-ref words (+ 2 n)))))
               (table-set! 3-words 3-s (+ 1 (table-ref 3-words 3-s 0))))))
    (display "\n\n****** 1 word:\n\n")
    (table-for-each
     (lambda (k v)
       (if (> v 3)
           (begin
             (set! 1-r (cons (list k v) 1-r))
             (display k) (display " : ") (display v) (newline))))
     1-word)
    (display "\n\n****** 2 words:\n\n")
    (table-for-each
     (lambda (k v)
       (if (> v 1)
           (begin
             (set! 2-r (cons (list k v) 2-r))
             (display k) (display " : ") (display v) (newline))))
     2-words)
    (display "\n\n****** 3 words:\n\n")
    (table-for-each
     (lambda (k v)
       (if (> v 1)
           (set! 3-r (cons (list k v) 3-r))
           (begin (display k) (display " : ") (display v) (newline))))
     3-words)
    

    (list (reverse 1-r) (reverse 2-r) (reverse 3-r))))


;; (key-phrases "data/testdata/climate_g8.txt")
;; (key-phrases "data/testdata/clinton_politics.txt")
