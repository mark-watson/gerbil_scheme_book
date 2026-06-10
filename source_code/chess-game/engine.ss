;; File: engine.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

(def logand bitwise-and)
(def logior bitwise-ior)
(def logxor bitwise-xor)
(def lognot bitwise-not)

(export EMPTY PAWN KNIGHT BISHOP ROOK QUEEN KING TYPE_MASK WHITE BLACK COLOR_MASK WK WQ BK BQ
        piece-type piece-color opponent color-idx
        square-names name-to-square
        knight-moves king-moves rook-rays bishop-rays queen-rays
        ZOBRIST_SIDE ZOBRIST_CASTLING ZOBRIST_EP zobrist-pieces
        move-struct make-move
        move-struct-from move-struct-to move-struct-piece-moved
        move-struct-piece-captured move-struct-promotion
        move-struct-is-en-passant move-struct-is-castling move-struct-is-double-push
        move-equals? move->uci
        board board-state make-board-state
        board-state-zobrist-hash board-state-en-passant-square board-state-castling-rights board-state-halfmove-clock
        board-grid board-pieces board-king-square board-turn board-castling-rights
        board-en-passant-square board-halfmove-clock board-fullmove-number board-zobrist-hash
        new-board board-reset! board-from-fen! board-to-fen board-compute-zobrist-hash
        is-square-attacked? in-check?
        get-pseudo-legal-moves get-legal-moves
        board-make-move! board-unmake-move!
        string-join string-contains-char? piece->char)

;; Pieces representation
(def EMPTY 0)
(def PAWN 1)
(def KNIGHT 2)
(def BISHOP 3)
(def ROOK 4)
(def QUEEN 5)
(def KING 6)
(def TYPE_MASK 7)

(def WHITE 8)
(def BLACK 16)
(def COLOR_MASK 24)

(def (piece-type p) (logand p TYPE_MASK))
(def (piece-color p) (logand p COLOR_MASK))
(def (opponent c) (if (= c WHITE) BLACK WHITE))
(def (color-idx c) (if (= c WHITE) 0 1))

;; Castling rights
(def WK 1)
(def WQ 2)
(def BK 4)
(def BQ 8)

;; Precomputed square names mapping 0-63 to a1..h8
(def square-names (make-vector 64))
(def name-to-square (make-hash-table))

(do ((row 0 (+ row 1)))
    ((= row 8))
  (do ((col 0 (+ col 1)))
      ((= col 8))
    (let* ((name (string-append (string (vector-ref '#(#\a #\b #\c #\d #\e #\f #\g #\h) col))
                                (string (vector-ref '#(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8) row))))
           (sq (+ (* row 8) col)))
      (vector-set! square-names sq name)
      (hash-put! name-to-square name sq))))

;; Precomputed move tables
(def knight-moves (make-vector 64 '()))
(def king-moves (make-vector 64 '()))
(def rook-rays (make-vector 64 '()))
(def bishop-rays (make-vector 64 '()))
(def queen-rays (make-vector 64 '()))

;; Fill precomputed moves
(do ((sq 0 (+ sq 1)))
    ((= sq 64))
  (let* ((r (quotient sq 8))
         (f (modulo sq 8)))
    
    ;; Knights
    (let ((km '()))
      (for-each
        (lambda (d)
          (let ((nr (+ r (car d)))
                (nf (+ f (cadr d))))
            (when (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
              (set! km (cons (+ (* nr 8) nf) km)))))
        '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))
      (vector-set! knight-moves sq (reverse km)))
    
    ;; Kings
    (let ((km '()))
      (for-each
        (lambda (d)
          (let ((nr (+ r (car d)))
                (nf (+ f (cadr d))))
            (when (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
              (set! km (cons (+ (* nr 8) nf) km)))))
        '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
      (vector-set! king-moves sq (reverse km)))
    
    ;; Rook rays
    (let ((rr '()))
      (for-each
        (lambda (d)
          (let ((dr (car d))
                (dc (cadr d))
                (ray '()))
            (let loop ((nr (+ r dr)) (nf (+ f dc)))
              (if (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
                (begin
                  (set! ray (cons (+ (* nr 8) nf) ray))
                  (loop (+ nr dr) (+ nf dc)))
                (set! rr (cons (reverse ray) rr))))))
        '((1 0) (-1 0) (0 1) (0 -1)))
      (vector-set! rook-rays sq (reverse rr)))
    
    ;; Bishop rays
    (let ((br '()))
      (for-each
        (lambda (d)
          (let ((dr (car d))
                (dc (cadr d))
                (ray '()))
            (let loop ((nr (+ r dr)) (nf (+ f dc)))
              (if (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
                (begin
                  (set! ray (cons (+ (* nr 8) nf) ray))
                  (loop (+ nr dr) (+ nf dc)))
                (set! br (cons (reverse ray) br))))))
        '((1 1) (1 -1) (-1 1) (-1 -1)))
      (vector-set! bishop-rays sq (reverse br)))
    
    ;; Queen rays (Rook + Bishop rays combined)
    (vector-set! queen-rays sq (append (vector-ref rook-rays sq) (vector-ref bishop-rays sq)))))

;; Zobrist hashing — deterministic LCG 64-bit generator
(def (create-rng seed)
  (let ((state (let ((val (or seed 1337)))
                 (if (even? val) (+ val 1) val))))
    (lambda ()
      (set! state (modulo (+ (* state 6364136223846793005) 1442695040888963407) 18446744073709551616))
      state)))

(def rng (create-rng 1337))
(def zobrist-pieces (make-vector 64))
(do ((sq 0 (+ sq 1)))
    ((= sq 64))
  (let ((v (make-vector 32)))
    (do ((p 0 (+ p 1)))
        ((= p 32))
      (vector-set! v p (rng)))
    (vector-set! zobrist-pieces sq v)))

(def ZOBRIST_SIDE (rng))
(def ZOBRIST_CASTLING (make-vector 16))
(do ((i 0 (+ i 1)))
    ((= i 16))
  (vector-set! ZOBRIST_CASTLING i (rng)))

(def ZOBRIST_EP (make-vector 64))
(do ((i 0 (+ i 1)))
    ((= i 64))
  (vector-set! ZOBRIST_EP i (rng)))

;; Move definition
(defstruct move-struct
  (from
   to
   piece-moved
   piece-captured
   promotion
   is-en-passant
   is-castling
   is-double-push)
  transparent: #t)

(def (make-move from to piece-moved (piece-captured EMPTY) (promotion EMPTY) (is-en-passant #f) (is-castling #f) (is-double-push #f))
  (make-move-struct from to piece-moved piece-captured promotion is-en-passant is-castling is-double-push))

(def (move-equals? m1 m2)
  (and (= (move-struct-from m1) (move-struct-from m2))
       (= (move-struct-to m1) (move-struct-to m2))
       (= (move-struct-promotion m1) (move-struct-promotion m2))))

(def (move->uci m)
  (let* ((from (move-struct-from m))
         (to (move-struct-to m))
         (promo (move-struct-promotion m))
         (from-name (vector-ref square-names from))
         (to-name (vector-ref square-names to))
         (base (string-append from-name to-name)))
    (if (> promo 0)
      (let ((pchar (cond
                     ((= promo QUEEN) "q")
                     ((= promo ROOK) "r")
                     ((= promo BISHOP) "b")
                     ((= promo KNIGHT) "n")
                     (else ""))))
        (string-append base pchar))
      base)))

;; BoardState snapshot for undo
(defstruct board-state
  (en-passant-square
   castling-rights
   halfmove-clock
   zobrist-hash)
  transparent: #t)

;; Board definition
(defstruct board
  (grid          ; 64-element vector of pieces
   pieces        ; vector of size 2 containing lists of square indices
   king-square   ; vector of size 2 containing king squares
   turn          ; WHITE or BLACK
   castling-rights
   en-passant-square
   halfmove-clock
   fullmove-number
   zobrist-hash)
  transparent: #t)

(def (new-board (fen #f))
  (let ((b (make-board
             (make-vector 64 EMPTY)
             (vector '() '())
             (make-vector 2 -1)
             WHITE
             (+ WK WQ BK BQ)
             -1
             0
             1
             0)))
    (if fen
      (board-from-fen! b fen)
      (board-reset! b))
    b))

;; String utilities
(def (string-join lst sep)
  (if (null? lst)
    ""
    (let loop ((rest (cdr lst)) (acc (car lst)))
      (if (null? rest)
        acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

(def (remove pred lst)
  (let loop ((lst lst) (acc '()))
    (cond
      ((null? lst) (reverse acc))
      ((pred (car lst)) (loop (cdr lst) acc))
      (else (loop (cdr lst) (cons (car lst) acc))))))

(def (string-contains-char? str char)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) char) #t)
        (else (loop (+ i 1)))))))

(def (piece->char p)
  (let ((pt (piece-type p))
        (pc (piece-color p)))
    (cond
      ((= pc WHITE)
       (cond
         ((= pt PAWN) "P")
         ((= pt KNIGHT) "N")
         ((= pt BISHOP) "B")
         ((= pt ROOK) "R")
         ((= pt QUEEN) "Q")
         ((= pt KING) "K")
         (else ".")))
      ((= pc BLACK)
       (cond
         ((= pt PAWN) "p")
         ((= pt KNIGHT) "n")
         ((= pt BISHOP) "b")
         ((= pt ROOK) "r")
         ((= pt QUEEN) "q")
         ((= pt KING) "k")
         (else ".")))
      (else "."))))

(def (find-king b color)
  (let* ((c-idx (color-idx color))
         (squares (vector-ref (board-pieces b) c-idx))
         (grid (board-grid b)))
    (let loop ((sqs squares))
      (cond
        ((null? sqs) -1)
        ((= (vector-ref grid (car sqs)) (logior color KING)) (car sqs))
        (else (loop (cdr sqs)))))))

(def (board-reset! b)
  (board-from-fen! b "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(def (board-from-fen! b fen)
  (let* ((parts (pregexp-split " " fen))
         (placement (list-ref parts 0))
         (active-color (list-ref parts 1))
         (castling (list-ref parts 2))
         (ep (list-ref parts 3))
         (halfmove (list-ref parts 4))
         (fullmove (list-ref parts 5))
         (grid (board-grid b))
         (pieces (board-pieces b))
         (king-sq (board-king-square b)))
    
    (vector-fill! grid EMPTY)
    (vector-set! pieces 0 '())
    (vector-set! pieces 1 '())
    
    (let loop ((i 0) (rank 7) (file 0))
      (when (< i (string-length placement))
        (let ((ch (string-ref placement i)))
          (cond
            ((char=? ch #\/)
             (loop (+ i 1) (- rank 1) 0))
            ((and (char>=? ch #\1) (char<=? ch #\8))
             (loop (+ i 1) rank (+ file (- (char->integer ch) (char->integer #\0)))))
            (else
             (let* ((color (if (char-upper-case? ch) WHITE BLACK))
                    (type (cond
                            ((char-ci=? ch #\p) PAWN)
                            ((char-ci=? ch #\n) KNIGHT)
                            ((char-ci=? ch #\b) BISHOP)
                            ((char-ci=? ch #\r) ROOK)
                            ((char-ci=? ch #\q) QUEEN)
                            ((char-ci=? ch #\k) KING)
                            (else EMPTY)))
                    (piece (logior color type))
                    (sq (+ (* rank 8) file))
                    (c-idx (color-idx color)))
               (vector-set! grid sq piece)
               (vector-set! pieces c-idx (cons sq (vector-ref pieces c-idx)))
               (loop (+ i 1) rank (+ file 1))))))))
    
    (set! (board-turn b) (if (string=? active-color "w") WHITE BLACK))
    
    (let ((rights 0))
      (when (string-contains-char? castling #\K) (set! rights (logior rights WK)))
      (when (string-contains-char? castling #\Q) (set! rights (logior rights WQ)))
      (when (string-contains-char? castling #\k) (set! rights (logior rights BK)))
      (when (string-contains-char? castling #\q) (set! rights (logior rights BQ)))
      (set! (board-castling-rights b) rights))
    
    (set! (board-en-passant-square b)
      (if (string=? ep "-")
        -1
        (hash-ref name-to-square ep -1)))
    
    (set! (board-halfmove-clock b) (string->number halfmove))
    (set! (board-fullmove-number b) (string->number fullmove))
    
    (vector-set! king-sq 0 (find-king b WHITE))
    (vector-set! king-sq 1 (find-king b BLACK))
    
    (set! (board-zobrist-hash b) (board-compute-zobrist-hash b))))

(def (board-to-fen b)
  (let ((ranks '())
        (grid (board-grid b)))
    (do ((rank 7 (- rank 1)))
        ((< rank 0))
      (let ((empty 0)
            (row ""))
        (do ((file 0 (+ file 1)))
            ((= file 8))
          (let* ((sq (+ (* rank 8) file))
                 (p (vector-ref grid sq)))
            (if (= p EMPTY)
              (set! empty (+ empty 1))
              (begin
                (when (> empty 0)
                  (set! row (string-append row (number->string empty)))
                  (set! empty 0))
                (set! row (string-append row (piece->char p)))))))
        (when (> empty 0)
          (set! row (string-append row (number->string empty))))
        (set! ranks (cons row ranks))))
    
    (let* ((ranks-str (string-join (reverse ranks) "/"))
           (turn-str (if (= (board-turn b) WHITE) "w" "b"))
           (castling-str "")
           (rights (board-castling-rights b))
           (ep-str (if (= (board-en-passant-square b) -1)
                     "-"
                     (vector-ref square-names (board-en-passant-square b)))))
      (when (> (logand rights WK) 0) (set! castling-str (string-append castling-str "K")))
      (when (> (logand rights WQ) 0) (set! castling-str (string-append castling-str "Q")))
      (when (> (logand rights BK) 0) (set! castling-str (string-append castling-str "k")))
      (when (> (logand rights BQ) 0) (set! castling-str (string-append castling-str "q")))
      (when (string=? castling-str "") (set! castling-str "-"))
      (format "~a ~a ~a ~a ~a ~a"
              ranks-str
              turn-str
              castling-str
              ep-str
              (board-halfmove-clock b)
              (board-fullmove-number b)))))

(def (board-compute-zobrist-hash b)
  (let ((h 0)
        (grid (board-grid b)))
    (do ((sq 0 (+ sq 1)))
        ((= sq 64))
      (let ((p (vector-ref grid sq)))
        (when (not (= p EMPTY))
          (set! h (logxor h (vector-ref (vector-ref zobrist-pieces sq) p))))))
    (when (= (board-turn b) BLACK)
      (set! h (logxor h ZOBRIST_SIDE)))
    (set! h (logxor h (vector-ref ZOBRIST_CASTLING (board-castling-rights b))))
    (let ((ep (board-en-passant-square b)))
      (when (not (= ep -1))
        (set! h (logxor h (vector-ref ZOBRIST_EP ep)))))
    h))

(def (is-square-attacked? b sq attacker-color)
  (let* ((grid (board-grid b))
         (r (quotient sq 8))
         (f (modulo sq 8))
         (opp-pawn-dir (if (= attacker-color WHITE) -8 8))
         (attacked #f))
    
    ;; 1. Pawn attacks
    (for-each
      (lambda (df)
        (let ((nf (+ f df)))
          (when (and (>= nf 0) (<= nf 7))
            (let ((pawn-sq (+ sq opp-pawn-dir df)))
              (when (and (>= pawn-sq 0) (< pawn-sq 64))
                (when (= (vector-ref grid pawn-sq) (logior attacker-color PAWN))
                  (set! attacked #t)))))))
      '(-1 1))
    
    (unless attacked
      ;; 2. Knight attacks
      (let loop ((targets (vector-ref knight-moves sq)))
        (when (not (null? targets))
          (if (= (vector-ref grid (car targets)) (logior attacker-color KNIGHT))
            (set! attacked #t)
            (loop (cdr targets))))))
    
    (unless attacked
      ;; 3. King attacks
      (let loop ((targets (vector-ref king-moves sq)))
        (when (not (null? targets))
          (if (= (vector-ref grid (car targets)) (logior attacker-color KING))
            (set! attacked #t)
            (loop (cdr targets))))))
    
    (unless attacked
      ;; 4. Rook/Queen rays
      (let loop-rays ((rays (vector-ref rook-rays sq)))
        (when (not (null? rays))
          (let loop-ray ((ray (car rays)))
            (when (not (null? ray))
              (let* ((t (car ray))
                     (p (vector-ref grid t)))
                (if (= p EMPTY)
                  (loop-ray (cdr ray))
                  (begin
                    (when (and (= (piece-color p) attacker-color)
                               (or (= (piece-type p) ROOK) (= (piece-type p) QUEEN)))
                      (set! attacked #t))
                    #f)))))
          (unless attacked
            (loop-rays (cdr rays))))))
    
    (unless attacked
      ;; 5. Bishop/Queen rays
      (let loop-rays ((rays (vector-ref bishop-rays sq)))
        (when (not (null? rays))
          (let loop-ray ((ray (car rays)))
            (when (not (null? ray))
              (let* ((t (car ray))
                     (p (vector-ref grid t)))
                (if (= p EMPTY)
                  (loop-ray (cdr ray))
                  (begin
                    (when (and (= (piece-color p) attacker-color)
                               (or (= (piece-type p) BISHOP) (= (piece-type p) QUEEN)))
                      (set! attacked #t))
                    #f)))))
          (unless attacked
            (loop-rays (cdr rays))))))
    
    attacked))

(def (in-check? b color)
  (let* ((c (or color (board-turn b)))
         (k-sq (vector-ref (board-king-square b) (color-idx c))))
    (is-square-attacked? b k-sq (opponent c))))

(def (get-pseudo-legal-moves b)
  (let* ((moves '())
         (color (board-turn b))
         (opp (opponent color))
         (forward (if (= color WHITE) 8 -8))
         (start-rank (if (= color WHITE) 1 6))
         (promo-rank (if (= color WHITE) 7 0))
         (promo-pieces (list QUEEN ROOK BISHOP KNIGHT))
         (grid (board-grid b))
         (pieces (vector-ref (board-pieces b) (color-idx color))))
    
    (for-each
      (lambda (sq)
        (let* ((piece (vector-ref grid sq))
               (ptype (piece-type piece))
               (r (quotient sq 8))
               (f (modulo sq 8)))
          (cond
            ((= ptype PAWN)
             (let ((push-sq (+ sq forward)))
               (when (= (vector-ref grid push-sq) EMPTY)
                 (if (= (quotient push-sq 8) promo-rank)
                   (for-each (lambda (pp) (set! moves (cons (make-move sq push-sq piece EMPTY pp) moves))) promo-pieces)
                   (begin
                     (set! moves (cons (make-move sq push-sq piece) moves))
                     (when (= r start-rank)
                       (let ((double-sq (+ push-sq forward)))
                         (when (= (vector-ref grid double-sq) EMPTY)
                           (set! moves (cons (make-move sq double-sq piece EMPTY EMPTY #f #f #t) moves)))))))))
             (for-each
               (lambda (df)
                 (let ((nf (+ f df)))
                   (when (and (>= nf 0) (<= nf 7))
                     (let* ((cap-sq (+ sq forward df))
                            (target (vector-ref grid cap-sq)))
                       (cond
                         ((and (not (= target EMPTY)) (= (piece-color target) opp))
                          (if (= (quotient cap-sq 8) promo-rank)
                            (for-each (lambda (pp) (set! moves (cons (make-move sq cap-sq piece target pp) moves))) promo-pieces)
                            (set! moves (cons (make-move sq cap-sq piece target) moves))))
                         ((= cap-sq (board-en-passant-square b))
                          (let ((ep-captured (vector-ref grid (- cap-sq forward))))
                            (set! moves (cons (make-move sq cap-sq piece ep-captured EMPTY #t) moves)))))))))
               '(-1 1)))
            
            ((= ptype KNIGHT)
             (for-each
               (lambda (t)
                 (let ((target (vector-ref grid t)))
                   (cond
                     ((= target EMPTY)
                      (set! moves (cons (make-move sq t piece) moves)))
                     ((= (piece-color target) opp)
                      (set! moves (cons (make-move sq t piece target) moves))))))
               (vector-ref knight-moves sq)))
            
            ((= ptype KING)
             (for-each
               (lambda (t)
                 (let ((target (vector-ref grid t)))
                   (cond
                     ((= target EMPTY)
                      (set! moves (cons (make-move sq t piece) moves)))
                     ((= (piece-color target) opp)
                      (set! moves (cons (make-move sq t piece target) moves))))))
               (vector-ref king-moves sq)))
            
            (else
             (let ((rays (cond
                           ((= ptype BISHOP) (vector-ref bishop-rays sq))
                           ((= ptype ROOK) (vector-ref rook-rays sq))
                           (else (vector-ref queen-rays sq)))))
               (for-each
                 (lambda (ray)
                   (let loop ((ray ray))
                     (when (not (null? ray))
                       (let* ((t (car ray))
                              (target (vector-ref grid t)))
                         (cond
                           ((= target EMPTY)
                            (set! moves (cons (make-move sq t piece) moves))
                            (loop (cdr ray)))
                           (else
                            (when (= (piece-color target) opp)
                              (set! moves (cons (make-move sq t piece target) moves)))
                            #f))))))
                 rays))))))
      pieces)
    
    ;; Add Castling
    (let ((rights (board-castling-rights b)))
      (if (= color WHITE)
        (when (= (vector-ref grid 4) (logior WHITE KING))
          (when (and (not (= (logand rights WK) 0))
                     (= (vector-ref grid 5) EMPTY)
                     (= (vector-ref grid 6) EMPTY))
            (when (and (not (is-square-attacked? b 4 opp))
                       (not (is-square-attacked? b 5 opp))
                       (not (is-square-attacked? b 6 opp)))
              (set! moves (cons (make-move 4 6 (logior WHITE KING) EMPTY EMPTY #f #t) moves))))
          (when (and (not (= (logand rights WQ) 0))
                     (= (vector-ref grid 3) EMPTY)
                     (= (vector-ref grid 2) EMPTY)
                     (= (vector-ref grid 1) EMPTY))
            (when (and (not (is-square-attacked? b 4 opp))
                       (not (is-square-attacked? b 3 opp))
                       (not (is-square-attacked? b 2 opp)))
              (set! moves (cons (make-move 4 2 (logior WHITE KING) EMPTY EMPTY #f #t) moves)))))
        (when (= (vector-ref grid 60) (logior BLACK KING))
          (when (and (not (= (logand rights BK) 0))
                     (= (vector-ref grid 61) EMPTY)
                     (= (vector-ref grid 62) EMPTY))
            (when (and (not (is-square-attacked? b 60 opp))
                       (not (is-square-attacked? b 61 opp))
                       (not (is-square-attacked? b 62 opp)))
              (set! moves (cons (make-move 60 62 (logior BLACK KING) EMPTY EMPTY #f #t) moves))))
          (when (and (not (= (logand rights BQ) 0))
                     (= (vector-ref grid 59) EMPTY)
                     (= (vector-ref grid 58) EMPTY)
                     (= (vector-ref grid 57) EMPTY))
            (when (and (not (is-square-attacked? b 60 opp))
                       (not (is-square-attacked? b 59 opp))
                       (not (is-square-attacked? b 58 opp)))
              (set! moves (cons (make-move 60 58 (logior BLACK KING) EMPTY EMPTY #f #t) moves)))))))
    
    moves))

(def (get-legal-moves b)
  (let ((legal '()))
    (for-each
      (lambda (move)
        (let* ((state (board-make-move! b move))
               (turn-color (board-turn b))
               (k-sq (vector-ref (board-king-square b) (color-idx (opponent turn-color)))))
          (when (not (is-square-attacked? b k-sq turn-color))
            (set! legal (cons move legal)))
          (board-unmake-move! b move state)))
      (get-pseudo-legal-moves b))
    (reverse legal)))

(def (board-make-move! b move)
  (let* ((state (make-board-state
                  (board-en-passant-square b)
                  (board-castling-rights b)
                  (board-halfmove-clock b)
                  (board-zobrist-hash b)))
         (grid (board-grid b))
         (pieces (board-pieces b))
         (king-sq (board-king-square b))
         (from (move-struct-from move))
         (to (move-struct-to move))
         (piece-moved (move-struct-piece-moved move))
         (piece-captured (move-struct-piece-captured move))
         (promotion (move-struct-promotion move))
         (is-ep (move-struct-is-en-passant move))
         (is-castling (move-struct-is-castling move))
         (is-double-push (move-struct-is-double-push move))
         
         (color (piece-color piece-moved))
         (opp (opponent color))
         (ci (color-idx color))
         (oi (color-idx opp))
         (h (board-zobrist-hash b)))
    
    (set! h (logxor h ZOBRIST_SIDE))
    (set! h (logxor h (vector-ref ZOBRIST_CASTLING (board-castling-rights b))))
    (let ((ep (board-en-passant-square b)))
      (when (not (= ep -1)) (set! h (logxor h (vector-ref ZOBRIST_EP ep)))))
    (set! h (logxor h (vector-ref (vector-ref zobrist-pieces from) piece-moved)))
    
    (vector-set! grid from EMPTY)
    (vector-set! pieces ci (remove (lambda (x) (= x from)) (vector-ref pieces ci)))
    
    (cond
      (is-ep
       (let* ((forward (if (= color WHITE) 8 -8))
              (captured-sq (- to forward))
              (captured-piece (vector-ref grid captured-sq)))
         (vector-set! grid captured-sq EMPTY)
         (vector-set! pieces oi (remove (lambda (x) (= x captured-sq)) (vector-ref pieces oi)))
         (set! h (logxor h (vector-ref (vector-ref zobrist-pieces captured-sq) captured-piece)))))
      ((not (= piece-captured EMPTY))
       (vector-set! grid to EMPTY)
       (vector-set! pieces oi (remove (lambda (x) (= x to)) (vector-ref pieces oi)))
       (set! h (logxor h (vector-ref (vector-ref zobrist-pieces to) piece-captured)))))
    
    (let ((final-piece (if (> promotion 0) (logior color promotion) piece-moved)))
      (vector-set! grid to final-piece)
      (vector-set! pieces ci (cons to (vector-ref pieces ci)))
      (set! h (logxor h (vector-ref (vector-ref zobrist-pieces to) final-piece)))
      
      (when (= (piece-type piece-moved) KING)
        (vector-set! king-sq ci to))
      
      (when is-castling
        (let ((rook-from -1) (rook-to -1))
          (cond
            ((= to 6)   (set! rook-from 7)  (set! rook-to 5))
            ((= to 2)   (set! rook-from 0)  (set! rook-to 3))
            ((= to 62)  (set! rook-from 63) (set! rook-to 61))
            (else       (set! rook-from 56) (set! rook-to 59)))
          (let ((rook (logior color ROOK)))
            (vector-set! grid rook-from EMPTY)
            (vector-set! pieces ci (remove (lambda (x) (= x rook-from)) (vector-ref pieces ci)))
            (set! h (logxor h (vector-ref (vector-ref zobrist-pieces rook-from) rook)))
            
            (vector-set! grid rook-to rook)
            (vector-set! pieces ci (cons rook-to (vector-ref pieces ci)))
            (set! h (logxor h (vector-ref (vector-ref zobrist-pieces rook-to) rook)))))))
    
    (let ((rights (board-castling-rights b)))
      (when (= (piece-type piece-moved) KING)
        (set! rights (logand rights (if (= color WHITE) (lognot (+ WK WQ)) (lognot (+ BK BQ))))))
      (when (or (= from 7) (= to 7)) (set! rights (logand rights (lognot WK))))
      (when (or (= from 0) (= to 0)) (set! rights (logand rights (lognot WQ))))
      (when (or (= from 63) (= to 63)) (set! rights (logand rights (lognot BK))))
      (when (or (= from 56) (= to 56)) (set! rights (logand rights (lognot BQ))))
      (set! (board-castling-rights b) rights))
    
    (set! (board-en-passant-square b)
      (if is-double-push
        (let ((forward (if (= color WHITE) 8 -8)))
          (- to forward))
        -1))
    
    (if (or (= (piece-type piece-moved) PAWN) (not (= piece-captured EMPTY)))
      (set! (board-halfmove-clock b) 0)
      (set! (board-halfmove-clock b) (+ (board-halfmove-clock b) 1)))
    
    (when (= color BLACK)
      (set! (board-fullmove-number b) (+ (board-fullmove-number b) 1)))
    
    (set! (board-turn b) opp)
    
    (set! h (logxor h (vector-ref ZOBRIST_CASTLING (board-castling-rights b))))
    (let ((ep (board-en-passant-square b)))
      (when (not (= ep -1)) (set! h (logxor h (vector-ref ZOBRIST_EP ep)))))
    
    (set! (board-zobrist-hash b) h)
    state))

(def (board-unmake-move! b move state)
  (let* ((grid (board-grid b))
         (pieces (board-pieces b))
         (king-sq (board-king-square b))
         (from (move-struct-from move))
         (to (move-struct-to move))
         (piece-moved (move-struct-piece-moved move))
         (piece-captured (move-struct-piece-captured move))
         (promotion (move-struct-promotion move))
         (is-ep (move-struct-is-en-passant move))
         (is-castling (move-struct-is-castling move))
         
         (color (piece-color piece-moved))
         (ci (color-idx color))
         (oi (color-idx (opponent color))))
    
    (set! (board-en-passant-square b) (board-state-en-passant-square state))
    (set! (board-castling-rights b) (board-state-castling-rights state))
    (set! (board-halfmove-clock b) (board-state-halfmove-clock state))
    (set! (board-zobrist-hash b) (board-state-zobrist-hash state))
    (set! (board-turn b) color)
    
    (when (= color BLACK)
      (set! (board-fullmove-number b) (- (board-fullmove-number b) 1)))
    
    (vector-set! grid to EMPTY)
    (vector-set! pieces ci (remove (lambda (x) (= x to)) (vector-ref pieces ci)))
    
    (vector-set! grid from piece-moved)
    (vector-set! pieces ci (cons from (vector-ref pieces ci)))
    
    (cond
      (is-ep
       (let* ((forward (if (= color WHITE) 8 -8))
              (captured-sq (- to forward)))
         (vector-set! grid captured-sq piece-captured)
         (vector-set! pieces oi (cons captured-sq (vector-ref pieces oi)))))
      ((not (= piece-captured EMPTY))
       (vector-set! grid to piece-captured)
       (vector-set! pieces oi (cons to (vector-ref pieces oi)))))
    
    (when (= (piece-type piece-moved) KING)
      (vector-set! king-sq ci from))
    
    (when is-castling
      (let ((rook-from -1) (rook-to -1))
        (cond
          ((= to 6)   (set! rook-from 7)  (set! rook-to 5))
          ((= to 2)   (set! rook-from 0)  (set! rook-to 3))
          ((= to 62)  (set! rook-from 63) (set! rook-to 61))
          (else       (set! rook-from 56) (set! rook-to 59)))
        (let ((rook (logior color ROOK)))
          (vector-set! grid rook-to EMPTY)
          (vector-set! pieces ci (remove (lambda (x) (= x rook-to)) (vector-ref pieces ci)))
          (vector-set! grid rook-from rook)
          (vector-set! pieces ci (cons rook-from (vector-ref pieces ci))))))))
