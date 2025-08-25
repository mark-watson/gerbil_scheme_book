;; FastTag.lisp
;;
;; Conversion of KnowledgeBooks.com Java FastTag to Scheme
;;
;; Copyright 2002 by Mark Watson.  All rights reserved.
;;

					;(define lex-hash
					;  (let ((hash (make-table)))
					;	(with-input-from-file
					;	    "data/tag.dat"
					;	  (lambda ()
					;	    (let loop ()
					;	      (let ((p (read)))
					;			(if (list? p) (table-set! hash (car p) (cadr p)))
					;			(if (not (eof-object? p)) (loop))))))
					;	hash))

(define (generate-lex-hash)
  (let ((count 0))
    (with-output-to-file "generated-code/tagdat.ss"
      (lambda ()
        (display "(define lex-hash (make-table))")
        (newline)
        (with-input-from-file
            "data/tag.dat"
          (lambda ()
            (let loop ()
              (let ((p (read)))
                (if (list? p) 
                    (let ()
                      (display "(table-set! lex-hash \"")
                      (display (car p))
                      (display "\" \"")
                      (display (caadr p))
                      (display "\")")
                      (newline)))
                (if (not (eof-object? p)) (loop))))))))))

;;
					; parts-of-speech
					;
					;  input: a vector of words (each a string)
					;  output: a vector of parts of speech
;;

(define (parts-of-speech words)
  (let ((ret '())
        (r #f)
        (lastRet #f)
        (lastWord #f))
    (for-each
     (lambda (w)
       (set! r (table-ref lex-hash w #f))
       ;; if this word is not in the hash table, try making it ll lower case:
       (if (not r)
           (set! r "NN"))
       ;;(if (list? r) (set! r (car r))))
       ;; apply transformation rules:

					; rule 1: DT, {VBD, VBP, VB} --> DT, NN
       (if (equal? lastRet "DT")
           (if (or
		(equal? r "VBD")
		(equal? r "VBP")
		(equal? r "VB"))
               (set! r "NN")))

					; rule 2: convert a noun to a number if a "." appears in the word
       (if (substring? "." w) (set! r "CD"))

					; rule 3: convert a noun to a past participle if word ends with "ed"
       (if (equal? (substring? "N" r) 0)
           (let ((i (string-suffix? "ed" w)))
             (if (equal? i (- (string-length w) 2))
                 (set! r "VBN"))))

					; rule 4: convert any type to an adverb if it ends with "ly"
       (let ((i (string-suffix? "ly" w)))
         (if (equal? i (- (string-length w) 2))
             (set! r "RB")))

					; rule 5: convert a common noun (NN or NNS) to an adjective
					;         if it ends with "al"
       (if (or
            (equal? r "NN")
            (equal? r "NNS"))
           (let ((i (string-suffix? "al" w)))
             (if (equal? i (- (string-length w) 2))
                 (set! r "RB"))))

					; rule 6: convert a noun to a verb if the receeding word is "would"
       (if (equal? (substring? "NN" r) 0)
           (if (equal? lastWord "would")
               (set! r "VB")))

					; rule 7: if a word has been categorized as a common noun and it
					;         ends with "s", then set its type to a plural noun (NNS)
       (if (equal? r "NN")
           (let ((i (string-suffix? "s" w)))
             (if (equal? i (- (string-length w) 1))
                 (set! r "NNS"))))

					; rule 8: convert a common noun to a present participle verb
					;         (i.e., a gerand)
       (if (equal? (substring? "NN" r) 0)
           (let ((i (string-suffix? "ing" w)))
             (if (equal? i (- (string-length w) 3))
                 (set! r "VBG"))))

       (set! lastRet ret)
       (set! lastWord w)
       (set! ret (cons r ret)))
     (vector->list words))  ;; not very efficient !!
    (list->vector (reverse ret))))


;; (parse (list->vector '("the" "cat" "ran")))
;; (parse (words-from-string "banking in Europe is a good business and a liberty"))


