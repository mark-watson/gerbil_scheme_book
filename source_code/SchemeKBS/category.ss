;; category.ss - categorization utilities

;;
;; get the category for words in a string
;;

(define (get-word-list-category words)
  (let ((v #f)
        (x #f)
        (ss #f)
        (cat-hash #f)
        ;;(tags #f)
        (word #f)
        (len #f)
        (len2 #f))

    (define (list-sort x)
      (define (test123 x y)
        (> (cadr x) (cadr y)))
      (sort x test123))

    ;;(set! words (words-from-string words))
    ;;(set! tags (parse words))
    (set! len (vector-length words))
    (set! len2 (length categoryHashtables))
    (set! v (make-vector len2))
    (do ((k 0 (+ k 1)))
        ((equal? k len))
      (set! word (string-downcase (vector-ref words k)))
      (do ((i 0 (+ i 1)))
          ((equal? i len2))
        (set! cat-hash (list-ref categoryHashtables i))
        (set! x (table-ref cat-hash word  #f))
        (if x
            (vector-set! v i (+ (vector-ref v i) x)))))
    (set! ss '())
    (do ((i 0 (+ i 1)))
        ((equal? i len2))
      (if (> (vector-ref v i) 0.01)
          (set! ss (cons (list (list-ref categoryNames i) (round (* (vector-ref v i) 10))) ss))))
    (set! ss (list-sort ss))
    (set! x (round (+ (/ (length ss) (/ len2 3)) 1)))
    (set! v '())
    (set! len2 (length ss))
    (do ((i 0 (+ i 1)))
        ((equal? i len2))
      (set! v (cons (list-ref ss i) v)))
    (reverse v)))

;; test:  (get-word-list-category (words-from-string "banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady."))


;;;;;;;; Utilities to make s-expression category file:

;;(define categoryHashtables '())
;;(define categoryNames '())
