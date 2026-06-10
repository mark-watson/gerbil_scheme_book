;; File: ai.ss
(import "engine")
(import :std/format
        :std/pregexp
        :std/srfi/95
        :gerbil/gambit)

(def logand bitwise-and)
(def logior bitwise-ior)
(def logxor bitwise-xor)
(def lognot bitwise-not)

(export evaluate-board
        is-endgame?
        get-best-move
        nodes-visited
        max-depth
        transposition-table
        TT_EXACT TT_ALPHA TT_BETA
        make-tt-entry tt-entry
        move-value
        search
        quiescence-search)

(def piece-values '#(0 100 320 330 500 900 20000))

(def PAWN_PST '#(
   0   0   0   0   0   0   0   0
  50  50  50  50  50  50  50  50
  10  10  20  30  30  20  10  10
   5   5  10  25  25  10   5   5
   0   0   0  20  20   0   0   0
   5  -5 -10   0   0 -10  -5   5
   5  10  10 -20 -20  10  10   5
   0   0   0   0   0   0   0   0
))

(def KNIGHT_PST '#(
  -50 -40 -30 -30 -30 -30 -40 -50
  -40 -20   0   0   0   0 -20 -40
  -30   0  10  15  15  10   0 -30
  -30   5  15  20  20  15   5 -30
  -30   0  15  20  20  15   0 -30
  -30   5  10  15  15  10   5 -30
  -40 -20   0   5   5   0 -20 -40
  -50 -40 -30 -30 -30 -30 -40 -50
))

(def BISHOP_PST '#(
  -20 -10 -10 -10 -10 -10 -10 -20
  -10   0   0   0   0   0   0 -10
  -10   0  10  10  10  10   0 -10
  -10   5   5  10  10   5   5 -10
  -10   0  10  10  10  10   0 -10
  -10  10  10  10  10  10  10 -10
  -10   5   0   0   0   0   5 -10
  -20 -10 -10 -10 -10 -10 -10 -20
))

(def ROOK_PST '#(
   0   0   0   0   0   0   0   0
   5  10  10  10  10  10  10   5
  -5   0   0   0   0   0   0  -5
  -5   0   0   0   0   0   0  -5
  -5   0   0   0   0   0   0  -5
  -5   0   0   0   0   0   0  -5
  -5   0   0   0   0   0   0  -5
   0   0   0   5   5   0   0   0
))

(def QUEEN_PST '#(
  -20 -10 -10  -5  -5 -10 -10 -20
  -10   0   0   0   0   0   0 -10
  -10   0   5   5   5   5   0 -10
   -5   0   5   5   5   5   0  -5
    0   0   5   5   5   5   0  -5
  -10   5   5   5   5   5   0 -10
  -10   0   5   0   0   0   0 -10
  -20 -10 -10  -5  -5 -10 -10 -20
))

(def KING_MIDDLE_PST '#(
  -30 -40 -40 -50 -50 -40 -40 -30
  -30 -40 -40 -50 -50 -40 -40 -30
  -30 -40 -40 -50 -50 -40 -40 -30
  -30 -40 -40 -50 -50 -40 -40 -30
  -20 -30 -30 -40 -40 -30 -30 -20
  -10 -20 -20 -20 -20 -20 -20 -10
   20  20   0   0   0   0  20  20
   20  30  10   0   0  10  30  20
))

(def KING_END_PST '#(
  -50 -40 -30 -20 -20 -30 -40 -50
  -30 -20 -10   0   0 -10 -20 -30
  -30 -10  20  30  30  20 -10 -30
  -30 -10  30  40  40  30 -10 -30
  -30 -10  30  40  40  30 -10 -30
  -30 -10  20  30  30  20 -10 -30
  -30 -30   0   0   0   0 -30 -30
  -50 -30 -30 -30 -30 -30 -30 -50
))

(def pst-tables (vector '#() PAWN_PST KNIGHT_PST BISHOP_PST ROOK_PST QUEEN_PST))

(def (is-endgame? b)
  (let ((material 0)
        (grid (board-grid b)))
    (for-each
      (lambda (sqs)
        (for-each
          (lambda (sq)
            (let ((pt (piece-type (vector-ref grid sq))))
              (when (and (not (= pt PAWN)) (not (= pt KING)))
                (set! material (+ material (vector-ref piece-values pt))))))
          sqs))
      (vector->list (board-pieces b)))
    (<= material 3000)))

(def (evaluate-board b)
  (let ((score 0)
        (grid (board-grid b))
        (pieces (board-pieces b)))
    
    ;; White pieces
    (for-each
      (lambda (sq)
        (let ((pt (piece-type (vector-ref grid sq))))
          (set! score (+ score (vector-ref piece-values pt)))
          (when (not (= pt KING))
            (set! score (+ score (vector-ref (vector-ref pst-tables pt) sq))))))
      (vector-ref pieces 0))
    
    ;; Black pieces
    (for-each
      (lambda (sq)
        (let ((pt (piece-type (vector-ref grid sq))))
          (set! score (- score (vector-ref piece-values pt)))
          (when (not (= pt KING))
            (set! score (- score (vector-ref (vector-ref pst-tables pt) (logxor sq 56)))))))
      (vector-ref pieces 1))
    
    ;; King PST
    (let* ((endgame (is-endgame? b))
           (king-table (if endgame KING_END_PST KING_MIDDLE_PST)))
      ;; White king
      (for-each
        (lambda (sq)
          (when (= (piece-type (vector-ref grid sq)) KING)
            (set! score (+ score (vector-ref king-table sq)))))
        (vector-ref pieces 0))
      ;; Black king
      (for-each
        (lambda (sq)
          (when (= (piece-type (vector-ref grid sq)) KING)
            (set! score (- score (vector-ref king-table (logxor sq 56))))))
        (vector-ref pieces 1)))
    
    (if (= (board-turn b) WHITE) score (- score))))

(def (move-value b move tt-move)
  (cond
    ((and tt-move (move-equals? move tt-move)) 1000000)
    ((> (move-struct-piece-captured move) 0)
     (- (+ 10000 (vector-ref piece-values (piece-type (move-struct-piece-captured move))))
        (quotient (vector-ref piece-values (piece-type (move-struct-piece-moved move))) 100)))
    ((> (move-struct-promotion move) 0)
     (+ 8000 (vector-ref piece-values (move-struct-promotion move))))
    ((move-struct-is-castling move) 1000)
    (else
     (let ((pt (piece-type (move-struct-piece-moved move)))
           (to (move-struct-to move))
           (from (move-struct-from move)))
       (if (not (= pt KING))
         (let ((pst (vector-ref pst-tables pt)))
           (- (vector-ref pst to) (vector-ref pst from)))
         0)))))

;; Transposition Table Constants and Struct
(def TT_EXACT 0)
(def TT_ALPHA 1)
(def TT_BETA 2)

(defstruct tt-entry
  (depth
   score
   flag
   best-move)
  transparent: #t)

(def transposition-table (make-hash-table))

(def max-depth 3)
(def nodes-visited 0)

(def (search b depth alpha beta)
  (set! nodes-visited (+ nodes-visited 1))
  (if (>= (board-halfmove-clock b) 100)
    0
    (let* ((tt-entry (hash-get transposition-table (board-zobrist-hash b)))
           (tt-best-move (and tt-entry (tt-entry-best-move tt-entry)))
           (original-alpha alpha))
      
      (when (and tt-entry (>= (tt-entry-depth tt-entry) depth))
        (let ((score (tt-entry-score tt-entry)))
          (cond
            ((> score 29000) (set! score (- score (- max-depth depth))))
            ((< score -29000) (set! score (+ score (- max-depth depth)))))
          
          (let ((flag (tt-entry-flag tt-entry)))
            (cond
              ((= flag TT_EXACT) (set! alpha score) (set! beta score))
              ((and (= flag TT_ALPHA) (<= score alpha)) (set! alpha score) (set! beta score))
              ((and (= flag TT_BETA) (>= score beta)) (set! alpha score) (set! beta score))
              ((= flag TT_ALPHA) (set! alpha (max alpha score)))
              ((= flag TT_BETA) (set! beta (min beta score)))))))
      
      (if (>= alpha beta)
        alpha
        (let ((legal-moves (get-legal-moves b)))
          (cond
            ((null? legal-moves)
             (if (in-check? b #f)
               (- -30000 (- max-depth depth))
               0))
            ((= depth 0)
             (quiescence-search b alpha beta))
            (else
             (let ((sorted-moves (sort legal-moves
                                       (lambda (m1 m2)
                                         (> (move-value b m1 tt-best-move)
                                            (move-value b m2 tt-best-move))))))
               (let loop ((moves sorted-moves) (best-score -1.0e9) (best-move #f) (current-alpha alpha))
                 (if (or (null? moves) (>= current-alpha beta))
                   (begin
                     (let ((flag (cond
                                   ((<= best-score original-alpha) TT_BETA)
                                   ((>= best-score beta) TT_ALPHA)
                                   (else TT_EXACT)))
                           (stored-score (cond
                                           ((> best-score 29000) (+ best-score (- max-depth depth)))
                                           ((< best-score -29000) (- best-score (- max-depth depth)))
                                           (else best-score))))
                       (let ((existing (hash-get transposition-table (board-zobrist-hash b))))
                         (when (or (not existing) (>= depth (tt-entry-depth existing)))
                           (hash-put! transposition-table
                                      (board-zobrist-hash b)
                                      (make-tt-entry depth stored-score flag best-move)))))
                     best-score)
                   (let* ((move (car moves))
                          (state (board-make-move! b move))
                          (score (- (search b (- depth 1) (- beta) (- current-alpha)))))
                     (board-unmake-move! b move state)
                     (let* ((new-best-score (max best-score score))
                            (new-best-move (if (> score best-score) move best-move))
                            (new-alpha (max current-alpha score)))
                       (loop (cdr moves) new-best-score new-best-move new-alpha)))))))))))))

(def (quiescence-search b alpha beta)
  (set! nodes-visited (+ nodes-visited 1))
  (let ((stand-pat (evaluate-board b)))
    (cond
      ((>= stand-pat beta) beta)
      (else
       (let ((current-alpha (max alpha stand-pat)))
         (let* ((all-pseudo (get-pseudo-legal-moves b))
                (captures '()))
           (for-each
             (lambda (m)
               (when (or (> (move-struct-piece-captured m) EMPTY) (> (move-struct-promotion m) EMPTY))
                 (let* ((state (board-make-move! b m))
                        (turn-color (board-turn b))
                        (k-sq (vector-ref (board-king-square b) (color-idx (opponent turn-color)))))
                   (when (not (is-square-attacked? b k-sq turn-color))
                     (set! captures (cons m captures)))
                   (board-unmake-move! b m state))))
             all-pseudo)
           (set! captures (reverse captures))
           (let ((sorted-caps (sort captures
                                    (lambda (m1 m2)
                                      (> (move-value b m1 #f)
                                         (move-value b m2 #f))))))
             (let loop ((moves sorted-caps) (curr-alpha current-alpha))
               (if (or (null? moves) (>= curr-alpha beta))
                 curr-alpha
                 (let* ((move (car moves))
                        (state (board-make-move! b move))
                        (score (- (quiescence-search b (- beta) (- curr-alpha)))))
                   (board-unmake-move! b move state)
                    (if (>= score beta)
                      beta
                      (loop (cdr moves) (max curr-alpha score)))))))))))))

(def (get-best-move b (depth 3))
  (when (> (table-length transposition-table) 500000)
    (set! transposition-table (make-hash-table)))
  
  (let ((best-move #f)
        (best-score 0))
    (do ((d 1 (+ d 1)))
        ((> d depth) (list best-move best-score))
      (set! max-depth d)
      (set! nodes-visited 0)
      
      (let* ((legal-moves (get-legal-moves b))
             (tt-entry (hash-get transposition-table (board-zobrist-hash b)))
             (tt-best (and tt-entry (tt-entry-best-move tt-entry)))
             (sorted-moves (sort legal-moves
                                 (lambda (m1 m2)
                                   (> (move-value b m1 tt-best)
                                      (move-value b m2 tt-best)))))
             (alpha -1.0e9)
             (beta 1.0e9)
             (current-best #f)
             (current-score -1.0e9))
        
        (for-each
          (lambda (move)
            (let* ((state (board-make-move! b move))
                   (score (- (search b (- d 1) (- beta) (- alpha)))))
              (board-unmake-move! b move state)
              (when (> score current-score)
                (set! current-score score)
                (set! current-best move))
              (set! alpha (max alpha score))))
          sorted-moves)
        
        (when current-best
          (set! best-move current-best)
          (set! best-score current-score)
          (hash-put! transposition-table
                     (board-zobrist-hash b)
                     (make-tt-entry d current-score TT_EXACT current-best)))))))
