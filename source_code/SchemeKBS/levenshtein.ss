;;; License

;; Copyright 2004 - 2005 Neil W. Van Dyke.  This program is Free
;; Software; you can redistribute it and/or modify it under the terms of the
;; GNU Lesser General Public License as published by the Free Software
;; Foundation; either version 2.1 of the License, or (at your option) any
;; later version.

;;; Description

;; This is a Scheme implementation of the Levenshtein Distance
;; algorithm, which is an edit distance metric of string similarity, due
;; to Vladimir Levenshtein.  The Levenshtein Distance is a function of two
;; strings that represents a count of single-character insertions, deletions,
;; and substitions that will change the first string to the second.

;;;; Example:
;;
;; (levenshtein "adresse" "addresxes")
;;

(define (%levenshtein:identity x) x)

(define (%levenshtein:string-empty? v) (zero? (string-length v)))
(define (%levenshtein:vector-empty? v) (zero? (vector-length v)))

(define (%levenshtein:string->vector s)
  (list->vector (string->list s)))

(define (vector-levenshtein/predicate/get-scratch a b pred get-scratch)
  (let ((a-len (vector-length a))
        (b-len (vector-length b)))
    (cond ((zero? a-len) b-len)
          ((zero? b-len) a-len)
          (else
           (let ((w    (get-scratch (+ 1 b-len)))
                 (next #f))
             (let fill ((k b-len))
               (vector-set! w k k)
               (or (zero? k) (fill (- k 1))))
             (let loop-i ((i 0))
               (if (= i a-len)
                   next
                   (let ((a-i (vector-ref a i)))
                     (let loop-j ((j   0)
                                  (cur (+ 1 i)))
                       (if (= j b-len)
                           (begin (vector-set! w b-len next)
                                  (loop-i (+ 1 i)))
                           ;; TODO: Make these costs parameters.
                           (begin (set! next (min (+ 1 (vector-ref w (+ 1 j)))
                                                  (+ 1 cur)
                                                  (if (pred a-i
                                                            (vector-ref b j))
                                                      (vector-ref w j)
                                                      (+ 1 (vector-ref w j)))))
                                  (vector-set! w j cur)
                                  (loop-j (+ 1 j) next))))))))))))


(define (vector-levenshtein/predicate a b pred)
  (vector-levenshtein/predicate/get-scratch a b pred make-vector))

(define (vector-levenshtein/eq    a b)
  (vector-levenshtein/predicate a b eq?))
(define (vector-levenshtein/eqv   a b)
  (vector-levenshtein/predicate a b eqv?))
(define (vector-levenshtein/equal a b)
  (vector-levenshtein/predicate a b equal?))

(define vector-levenshtein vector-levenshtein/equal)

(define (list-levenshtein/predicate a b pred)
  (cond ((null? a) (length b))
        ((null? b) (length a))
        (else (vector-levenshtein/predicate (list->vector a)
                                            (list->vector b)
                                            pred))))

(define (list-levenshtein/eq    a b) (list-levenshtein/predicate a b eq?))
(define (list-levenshtein/eqv   a b) (list-levenshtein/predicate a b eqv?))
(define (list-levenshtein/equal a b) (list-levenshtein/predicate a b equal?))

(define list-levenshtein list-levenshtein/equal)

(define (string-levenshtein a b)
  ;; TODO: Maybe make a version that doesn't convert to vectors but also
  ;;       doesn't do lots of string-refs.
  (cond ((zero? (string-length a)) (string-length b))
        ((zero? (string-length b)) (string-length a))
        (else (vector-levenshtein/eqv
               (%levenshtein:string->vector a)
               (%levenshtein:string->vector b)))))

(define (%levenshtein:string-levenshtein/predicate a b pred)
  (cond ((zero? (string-length a)) (string-length b))
        ((zero? (string-length b)) (string-length a))
        (else (vector-levenshtein/predicate
               (%levenshtein:string->vector a)
               (%levenshtein:string->vector b)
               pred))))

(define levenshtein/predicate
  ;; TODO: Change this to a let-syntax.
  (let ((foo (lambda (a b pred a-emp a-len a-vec)
               (let ((bar (lambda (b-emp b-len b-vec)
                            (if (b-emp b)
                                (a-len a)
                                (vector-levenshtein/predicate (a-vec a)
                                                              (b-vec b)
                                                              pred)))))
                 (cond ((vector? b) (bar %levenshtein:vector-empty?
                                         vector-length
                                         %levenshtein:identity))
                       ((string? b) (bar %levenshtein:string-empty?
                                         string-length
                                         %levenshtein:string->vector))
                       ((list?   b) (bar null? length list->vector))
                       (else (error "term 2 must be vector, list, or string:"
                                    b)))))))
    (lambda (a b pred)
      (cond ((vector? a) (if (vector? b)
                             (vector-levenshtein/predicate a b pred)
                             (foo a b pred
                                  %levenshtein:vector-empty?
                                  vector-length
                                  %levenshtein:identity)))
            ((string? a) (if (string? b)
                             (%levenshtein:string-levenshtein/predicate
                              a b pred)
                             (foo a b pred
                                  %levenshtein:string-empty?
                                  string-length
                                  %levenshtein:string->vector)))
            ((list?   a) (if (list? b)
                             (list-levenshtein/predicate a b pred)
                             (foo a b pred null? length list->vector)))
            (else (error "term 1 must be vector, list, or string:" a))))))

(define (levenshtein a b)
  (if (and (string? a) (string? b))
      (string-levenshtein a b)
      (levenshtein/predicate a b equal?)))

