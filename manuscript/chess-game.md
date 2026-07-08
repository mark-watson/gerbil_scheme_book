# Building a Chess Engine and AI Bot

Chess has served as a benchmark problem for artificial intelligence since the very birth of the field. Alan Turing described a paper chess-playing algorithm in 1950, and Claude Shannon published the foundational framework for computer chess the same year. Today, engines like Stockfish and Leela Chess Zero play at levels far beyond any human grandmaster, but the algorithmic ideas they are built upon — board representation, legal move generation, alpha-beta search, and positional evaluation — are surprisingly accessible. Building a chess engine from scratch is one of the most complete exercises in applied computer science: it demands careful data structure design, recursive search, hashing, and incremental state management all in a single coherent program.

In this chapter we implement a fully playable chess engine and AI bot in Gerbil Scheme — three modules totalling roughly 900 lines of code. The engine correctly handles every rule of chess, including en passant, castling, pawn promotion, and the fifty-move draw rule. The AI opponent uses iterative deepening negamax search with alpha-beta pruning, a transposition table for caching previously evaluated positions, a quiescence search to avoid the horizon effect on captures, and piece-square tables that encode positional knowledge. The interactive command-line interface renders a coloured Unicode board in the terminal and accepts moves in Universal Chess Interface (UCI) notation.

## Theoretical Background

### Board Representation

The first design decision in any chess engine is how to represent the board in memory. The two dominant approaches are **bitboards** (one 64-bit integer per piece type, with bits corresponding to occupied squares) and **mailbox** representation (an array mapping square index to piece). Bitboards enable parallelism via bitwise operations and are used in professional engines, but they make the code significantly harder to read. For this chapter we use an 8×8 mailbox encoded as a flat 64-element vector. Each element holds a small integer encoding both the piece type (bits 0–2) and the piece color (bits 3–4) using bitwise OR:

```
piece = color | type
```

The type codes are 0 (empty) through 6 (king). White is encoded as 8 and black as 16. To extract the type from a piece value `p` we compute `p AND 7`; to extract the color we compute `p AND 24`. This simple two-field encoding eliminates branch-heavy conditionals and maps naturally to Gerbil's `bitwise-and`, `bitwise-ior`, and `bitwise-xor` primitives.

In addition to the board grid, the engine maintains an **active piece list** — two lists of occupied square indices, one per color. This means that when generating moves for white, the engine iterates only over white's active squares rather than all 64 squares, a constant-factor speedup that matters at high search depths.

### Precomputed Move Tables

A naive move generator re-derives the set of reachable squares for each piece on every call. Precomputed tables eliminate that work by computing, at startup, every possible knight target, king target, and sliding-piece ray from each of the 64 squares. A **ray** is an ordered list of squares in one direction from a given square, stopping at the board edge. During move generation the engine walks each ray until it hits a friendly piece (stop) or an enemy piece (stop after capture). Because the tables are fixed arrays, this lookup is O(1) per ray and allocates no new memory.

### Zobrist Hashing

Searching a chess position requires quickly comparing positions to detect repetitions and to look up previously evaluated results in a transposition table. A naive equality check over all 64 squares is O(64); Zobrist hashing reduces this to O(1) by maintaining a 64-bit integer that uniquely identifies (with overwhelmingly high probability) the current board state.

The Zobrist scheme assigns a random 64-bit number to each (square, piece) combination at startup — 64 squares times 32 possible piece encodings gives 2048 numbers. The board hash is the XOR of all the numbers corresponding to occupied squares. Critically, XOR is self-inverse: making a move that removes a piece from square `s` simply XORs out `zobrist[s][piece]`, and placing the new piece XORs in the new value. Additional random numbers encode whose turn it is, the four castling rights, and the en-passant file. This incremental update strategy keeps the hash cost per move to a handful of XOR operations.

### Negamax Search with Alpha-Beta Pruning

A chess engine evaluates a position by searching forward through possible move sequences. The **minimax** algorithm assigns a score to the root position by assuming both players play optimally: the side to move maximizes the score, while the opponent minimizes it. **Negamax** is a clean reformulation of minimax that exploits the zero-sum nature of chess: the score for the side to move equals the negation of the score for the opponent. Every recursive call always maximizes, and scores are negated at each level.

Searching the full game tree is computationally infeasible. **Alpha-beta pruning** cuts large portions of the tree without affecting the result. Two bounds are passed through the recursion:

- `alpha` — the best score the maximizer can guarantee so far.
- `beta` — the best score the minimizer can guarantee so far.

Whenever `alpha >= beta`, the current subtree cannot influence the final result, and search is abandoned. In the best case, with perfect move ordering, alpha-beta reduces the effective branching factor from `b` to `sqrt(b)`, roughly doubling the searchable depth within the same time budget.

**Iterative deepening** runs alpha-beta repeatedly at depths 1, 2, 3, … up to the target depth. At each depth, the best move found is stored in the transposition table and used to order moves at the next depth. Because better-ordered moves produce more cutoffs, iterative deepening with transposition table hints typically outperforms a single deep search in the same time window.

**Quiescence search** is a fix for the "horizon effect": at the maximum depth, a position may look quiet but have a hanging piece about to be captured. The quiescence extension continues the search past the nominal depth, but only for captures and promotions. It terminates when the board is "quiet" (no forcing tactics available), making the static evaluation far more reliable.

### Static Evaluation

When search reaches a leaf node, it calls a static evaluation function that returns a score in centipawns (one hundredth of a pawn value). The evaluation in this engine has two components:

1. **Material count** — the sum of piece values: pawn=100, knight=320, bishop=330, rook=500, queen=900, king=20000 (effectively infinite).
2. **Piece-square table (PST) bonuses** — each piece type has an 8×8 table of bonus/penalty values expressing positional preferences. Knights are penalized on the rim and rewarded in the center. Pawns are rewarded for advancement toward the seventh rank. Kings are penalized for exposure in the middlegame but rewarded for active central play in the endgame.

The engine detects the endgame by summing non-pawn, non-king material; when it falls below 3000 centipawns, the king PST switches to an endgame table that favors central activity.

## Project Structure

The project directory `source_code/chess-game` contains the following files:

| File | Description |
|------|-------------|
| `engine.ss` | Board representation, precomputed move tables, Zobrist hashing, FEN parsing, pseudo-legal and legal move generation, and the make/unmake move machinery. |
| `ai.ss` | Static evaluation with PSTs, move ordering heuristics, iterative deepening negamax with alpha-beta pruning, quiescence search, and the transposition table. |
| `cli.ss` | Interactive command-line interface: Unicode board rendering with ANSI colors, UCI move parsing, and the main game loop. |
| `perft.ss` | Correctness and performance verification using Perft, a standard move-count benchmark for chess engines. |
| `Makefile` | Convenience targets for running, testing, and compiling the project. |
| `gerbil.pkg` | Package declaration for the Gerbil module system. |

## The Engine Module: engine.ss

The engine module is the foundation of the entire project. It exports every primitive needed by the AI and CLI layers: piece constants, board structures, move structures, move generators, and the make/unmake functions.

### Piece Encoding and Aliases

The module begins by aliasing Gambit's long-form bitwise function names to shorter Lisp-style names, then defining the piece encoding constants.

```scheme
;; File: engine.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

(def logand bitwise-and)
(def logior bitwise-ior)
(def logxor bitwise-xor)
(def lognot bitwise-not)

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

;; Castling rights (stored as a bitmask)
(def WK 1)   ; White kingside
(def WQ 2)   ; White queenside
(def BK 4)   ; Black kingside
(def BQ 8)   ; Black queenside
```

The `piece-type` and `piece-color` helpers simply mask out the relevant bits of a combined piece integer. For example, a white queen is encoded as `logior WHITE QUEEN` = `logior 8 5` = `13`. Calling `piece-type 13` returns `logand 13 7` = `5` (QUEEN), and `piece-color 13` returns `logand 13 24` = `8` (WHITE). Castling rights are stored as a four-bit mask: bit 0 = white kingside, bit 1 = white queenside, bit 2 = black kingside, bit 3 = black queenside.

### Square Names and Precomputed Tables

```scheme
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
```

The board uses a linear index where square `sq = rank * 8 + file`, with rank 0 being the first rank (White's back rank) and file 0 being the `a` file. The `square-names` vector maps 0–63 to UCI strings like `"a1"`, and `name-to-square` is the reverse hash table.

The precomputed tables are filled by a startup loop. For knights and kings the loop generates the short fixed offsets. For sliding pieces (rook, bishop, queen), it generates **rays** — ordered lists of squares in each direction, stopping at the board boundary:

```scheme
(do ((sq 0 (+ sq 1)))
    ((= sq 64))
  (let* ((r (quotient sq 8))
         (f (modulo sq 8)))
    ;; Knight targets
    (let ((km '()))
      (for-each
        (lambda (d)
          (let ((nr (+ r (car d)))
                (nf (+ f (cadr d))))
            (when (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
              (set! km (cons (+ (* nr 8) nf) km)))))
        '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))
      (vector-set! knight-moves sq (reverse km)))

    ;; Rook rays (north, south, east, west)
    (let ((rr '()))
      (for-each
        (lambda (d)
          (let ((dr (car d)) (dc (cadr d)) (ray '()))
            (let loop ((nr (+ r dr)) (nf (+ f dc)))
              (if (and (>= nr 0) (< nr 8) (>= nf 0) (< nf 8))
                (begin
                  (set! ray (cons (+ (* nr 8) nf) ray))
                  (loop (+ nr dr) (+ nf dc)))
                (set! rr (cons (reverse ray) rr))))))
        '((1 0) (-1 0) (0 1) (0 -1)))
      (vector-set! rook-rays sq (reverse rr)))
    ;; ... bishop and queen rays follow same pattern
    ))
```

Each element of `rook-rays` at index `sq` is a list of four rays, each ray being a list of square indices in order from `sq` outward. When the move generator walks a ray and hits a piece, it stops — no squares past that piece are accessible.

### Zobrist Hashing

The engine uses a deterministic 64-bit linear congruential generator (LCG) to produce the Zobrist random numbers at startup. Using a seeded RNG ensures that the numbers are identical across runs (important for debugging) without requiring a hardcoded table.

```scheme
(def (create-rng seed)
  (let ((state (let ((val (or seed 1337)))
                 (if (even? val) (+ val 1) val))))
    (lambda ()
      (set! state (modulo (+ (* state 6364136223846793005) 1442695040888963407) 18446744073709551616))
      state)))

(def rng (create-rng 1337))

;; One 32-element vector per square (indexed by piece encoding 0-31)
(def zobrist-pieces (make-vector 64))
(do ((sq 0 (+ sq 1)))
    ((= sq 64))
  (let ((v (make-vector 32)))
    (do ((p 0 (+ p 1)))
        ((= p 32))
      (vector-set! v p (rng)))
    (vector-set! zobrist-pieces sq v)))

(def ZOBRIST_SIDE (rng))
(def ZOBRIST_CASTLING (make-vector 16))   ; indexed by 4-bit castling mask
(def ZOBRIST_EP (make-vector 64))         ; indexed by en-passant square
```

The `board-compute-zobrist-hash` function builds the full hash from scratch by iterating over occupied squares. In practice this is only called once, when a position is loaded from FEN. All subsequent hash updates are incremental — the `board-make-move!` function XORs out moved/captured pieces and XORs in their new values before updating the board state.

### Board and Move Structures

Gerbil's `defstruct` macro creates transparent structures with automatic accessors and setters. The move structure records every attribute needed to fully undo a move:

```scheme
(defstruct move-struct
  (from to piece-moved piece-captured promotion
   is-en-passant is-castling is-double-push)
  transparent: #t)

(def (make-move from to piece-moved
                (piece-captured EMPTY) (promotion EMPTY)
                (is-en-passant #f) (is-castling #f) (is-double-push #f))
  (make-move-struct from to piece-moved piece-captured promotion
                    is-en-passant is-castling is-double-push))
```

The board structure holds the complete game state. A separate `board-state` snapshot records the fields that are difficult to recompute during undo (the Zobrist hash, castling rights, en-passant square, and half-move clock):

```scheme
(defstruct board-state
  (en-passant-square castling-rights halfmove-clock zobrist-hash)
  transparent: #t)

(defstruct board
  (grid pieces king-square turn castling-rights
   en-passant-square halfmove-clock fullmove-number zobrist-hash)
  transparent: #t)
```

### FEN Parsing

The Forsyth-Edwards Notation (FEN) is the standard format for encoding chess positions as strings. A typical starting position FEN is:

```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
```

The six space-separated fields encode: piece placement (ranks 8 to 1, files a to h), active color, castling availability, en passant target square, half-move clock, and full-move number. Uppercase letters are white pieces, lowercase are black.

The `board-from-fen!` function mutates an existing board in place, making it suitable for use as a reset operation. It uses `pregexp-split` to tokenize the FEN string, then walks the piece placement character by character:

```scheme
(def (board-from-fen! b fen)
  (let* ((parts (pregexp-split " " fen))
         (placement (list-ref parts 0))
         ...)
    (let loop ((i 0) (rank 7) (file 0))
      (when (< i (string-length placement))
        (let ((ch (string-ref placement i)))
          (cond
            ((char=? ch #\/) (loop (+ i 1) (- rank 1) 0))
            ((and (char>=? ch #\1) (char<=? ch #\8))
             (loop (+ i 1) rank (+ file (- (char->integer ch) (char->integer #\0)))))
            (else
             (let* ((color (if (char-upper-case? ch) WHITE BLACK))
                    (type (cond
                            ((char-ci=? ch #\p) PAWN)
                            ((char-ci=? ch #\n) KNIGHT)
                            ...))
                    (piece (logior color type))
                    (sq (+ (* rank 8) file)))
               (vector-set! grid sq piece)
               (loop (+ i 1) rank (+ file 1))))))))))
```

### Legal Move Generation

Move generation is a two-phase process. First, `get-pseudo-legal-moves` generates all moves that obey piece movement rules but may leave the king in check. Then `get-legal-moves` filters this list by actually making each move, checking whether the king is attacked after the move, and unmaking it.

The pseudo-legal generator iterates over the active piece list for the side to move and dispatches on piece type:

```scheme
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
               (ptype (piece-type piece)))
          (cond
            ((= ptype PAWN) ...)   ; single push, double push, captures, en passant, promotion
            ((= ptype KNIGHT) ...) ; lookup in knight-moves table
            ((= ptype KING) ...)   ; lookup in king-moves table
            (else ...))))          ; sliding pieces: walk each ray until blocked
      pieces)
    moves))
```

The pawn case is the most complex, handling both pushes and diagonal captures, each possibly resulting in four promotion moves if the destination rank is the back rank. En-passant captures store the captured pawn's square (not the destination square) in `piece-captured` by looking one square behind the en-passant target.

Castling is appended after the main loop. The engine checks that the king and rook are on their original squares, the intermediate squares are empty, and none of the king's transit squares are under attack:

```scheme
;; White kingside castling
(when (and (not (= (logand rights WK) 0))
           (= (vector-ref grid 5) EMPTY)
           (= (vector-ref grid 6) EMPTY)
           (not (is-square-attacked? b 4 opp))
           (not (is-square-attacked? b 5 opp))
           (not (is-square-attacked? b 6 opp)))
  (set! moves (cons (make-move 4 6 (logior WHITE KING) EMPTY EMPTY #f #t) moves)))
```

The `is-square-attacked?` function checks all attack vectors from a given square: pawn attacks (reverse-engineering from the target's perspective), knight jumps, king proximity, and sliding rays for rooks/queens and bishops/queens.

### Make and Unmake Move

The `board-make-move!` function returns a `board-state` snapshot before modifying the board. The `board-unmake-move!` function restores the board using that snapshot. This design avoids allocating a new board object for every node in the search tree.

```scheme
(def (board-make-move! b move)
  (let* ((state (make-board-state
                  (board-en-passant-square b)
                  (board-castling-rights b)
                  (board-halfmove-clock b)
                  (board-zobrist-hash b)))
         ...)
    ;; Incrementally update Zobrist hash
    (set! h (logxor h ZOBRIST_SIDE))
    (set! h (logxor h (vector-ref ZOBRIST_CASTLING (board-castling-rights b))))
    (set! h (logxor h (vector-ref (vector-ref zobrist-pieces from) piece-moved)))
    ;; Update grid and piece lists
    (vector-set! grid from EMPTY)
    (vector-set! pieces ci (remove (lambda (x) (= x from)) (vector-ref pieces ci)))
    ;; ... handle en passant, regular captures, promotion, castling
    state))

(def (board-unmake-move! b move state)
  ;; Restore fields from snapshot
  (set! (board-en-passant-square b) (board-state-en-passant-square state))
  (set! (board-castling-rights b) (board-state-castling-rights state))
  (set! (board-halfmove-clock b) (board-state-halfmove-clock state))
  (set! (board-zobrist-hash b) (board-state-zobrist-hash state))
  ;; Reverse all grid and piece-list mutations
  ...)
```

The Zobrist hash is restored simply by copying it from the snapshot — there is no need to re-derive it, since it was saved before any move was applied. Castling unmake moves the rook back from its castled position; en-passant unmake restores the captured pawn to its original square.

## The AI Module: ai.ss

The AI module imports the engine and adds everything needed to select a strong move: position evaluation, move ordering, and the search algorithm.

### Piece-Square Tables

Each piece type has an 8×8 table of centipawn bonus/penalty values expressing positional preferences. The tables are written from White's perspective (rank 1 at the bottom), and Black's scores are looked up with `logxor sq 56` — a simple XOR that mirrors the square index vertically.

```scheme
;; File: ai.ss
(import "engine")

(def piece-values '#(0 100 320 330 500 900 20000))

(def PAWN_PST '#(
   0   0   0   0   0   0   0   0   ; rank 8 (promotion rank)
  50  50  50  50  50  50  50  50   ; rank 7
  10  10  20  30  30  20  10  10
   5   5  10  25  25  10   5   5
   0   0   0  20  20   0   0   0
   5  -5 -10   0   0 -10  -5   5
   5  10  10 -20 -20  10  10   5
   0   0   0   0   0   0   0   0   ; rank 1 (starting rank)
))
```

The PAWN_PST rewards pawns that have advanced toward promotion, bonuses pawns on the d and e files in the center, and penalizes doubled pawns in front of the king. The KNIGHT_PST heavily penalizes edge squares (`-50` in the corners) and rewards the central cluster. Two separate king tables, `KING_MIDDLE_PST` and `KING_END_PST`, are applied based on whether the position is an endgame (total non-pawn, non-king material below 3000 centipawns):

```scheme
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
```

### The Evaluation Function

The static evaluator returns a score in centipawns from the perspective of the side to move. Positive is good for the moving side, negative is bad:

```scheme
(def (evaluate-board b)
  (let ((score 0)
        (grid (board-grid b))
        (pieces (board-pieces b)))
    ;; White pieces: add material + PST bonus
    (for-each
      (lambda (sq)
        (let ((pt (piece-type (vector-ref grid sq))))
          (set! score (+ score (vector-ref piece-values pt)))
          (when (not (= pt KING))
            (set! score (+ score (vector-ref (vector-ref pst-tables pt) sq))))))
      (vector-ref pieces 0))
    ;; Black pieces: subtract material + PST bonus (mirrored square)
    (for-each
      (lambda (sq)
        (let ((pt (piece-type (vector-ref grid sq))))
          (set! score (- score (vector-ref piece-values pt)))
          (when (not (= pt KING))
            (set! score (- score (vector-ref (vector-ref pst-tables pt) (logxor sq 56)))))))
      (vector-ref pieces 1))
    ;; King PST (phase-dependent)
    ...
    (if (= (board-turn b) WHITE) score (- score))))
```

The final line negates the score when black is to move, converting from an absolute white-perspective score to a side-to-move-perspective score as required by negamax.

### Move Ordering

Alpha-beta pruning is most effective when the best move is searched first. The `move-value` function assigns a priority to each move before sorting:

```scheme
(def (move-value b move tt-move)
  (cond
    ;; Transposition table best move gets highest priority
    ((and tt-move (move-equals? move tt-move)) 1000000)
    ;; Captures ordered by MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
    ((> (move-struct-piece-captured move) 0)
     (- (+ 10000 (vector-ref piece-values (piece-type (move-struct-piece-captured move))))
        (quotient (vector-ref piece-values (piece-type (move-struct-piece-moved move))) 100)))
    ;; Promotions (queening scores highest)
    ((> (move-struct-promotion move) 0)
     (+ 8000 (vector-ref piece-values (move-struct-promotion move))))
    ;; Castling
    ((move-struct-is-castling move) 1000)
    ;; Quiet moves: score by PST delta (improvement in positional value)
    (else
     (let ((pt (piece-type (move-struct-piece-moved move))))
       (if (not (= pt KING))
         (let ((pst (vector-ref pst-tables pt)))
           (- (vector-ref pst (move-struct-to move))
              (vector-ref pst (move-struct-from move))))
         0)))))
```

MVV-LVA (Most Valuable Victim, Least Valuable Attacker) is a classic heuristic: capturing a queen with a pawn scores much higher than capturing a pawn with a queen, both because the queen is worth more and because risking a queen to capture a pawn is strategically suspect.

### Transposition Table

The transposition table is a hash table mapping Zobrist hashes to cached search results. Each entry records the search depth, the score, a flag indicating whether the score is exact or a bound, and the best move found:

```scheme
(def TT_EXACT 0)   ; Score is exact
(def TT_ALPHA 1)   ; Score is an upper bound (failed low)
(def TT_BETA  2)   ; Score is a lower bound (failed high)

(defstruct tt-entry (depth score flag best-move) transparent: #t)

(def transposition-table (make-hash-table))
```

When the table has more than 500,000 entries, it is flushed to prevent memory exhaustion between games.

### Negamax Search

The core search function implements negamax with alpha-beta pruning and transposition table lookup:

```scheme
(def (search b depth alpha beta)
  (set! nodes-visited (+ nodes-visited 1))
  (if (>= (board-halfmove-clock b) 100)
    0  ; 50-move rule draw
    (let* ((tt-entry (hash-get transposition-table (board-zobrist-hash b)))
           (tt-best-move (and tt-entry (tt-entry-best-move tt-entry)))
           (original-alpha alpha))
      ;; Probe transposition table
      (when (and tt-entry (>= (tt-entry-depth tt-entry) depth))
        (let ((score (tt-entry-score tt-entry))
              (flag (tt-entry-flag tt-entry)))
          (cond
            ((= flag TT_EXACT) (set! alpha score) (set! beta score))
            ((= flag TT_ALPHA) (set! alpha (max alpha score)))
            ((= flag TT_BETA)  (set! beta (min beta score))))))
      
      (if (>= alpha beta)
        alpha  ; TT cutoff
        (let ((legal-moves (get-legal-moves b)))
          (cond
            ;; Terminal: checkmate or stalemate
            ((null? legal-moves)
             (if (in-check? b #f) (- -30000 (- max-depth depth)) 0))
            ;; Leaf: quiescence search
            ((= depth 0)
             (quiescence-search b alpha beta))
            ;; Interior: sort moves and recurse
            (else
             (let ((sorted-moves (sort legal-moves
                                       (lambda (m1 m2)
                                         (> (move-value b m1 tt-best-move)
                                            (move-value b m2 tt-best-move))))))
               (let loop ((moves sorted-moves) (best-score -1.0e9) (best-move #f) (current-alpha alpha))
                 (if (or (null? moves) (>= current-alpha beta))
                   (begin
                     ;; Store result in transposition table
                     (let ((flag (cond
                                   ((<= best-score original-alpha) TT_BETA)
                                   ((>= best-score beta) TT_ALPHA)
                                   (else TT_EXACT))))
                       (hash-put! transposition-table (board-zobrist-hash b)
                                  (make-tt-entry depth best-score flag best-move)))
                     best-score)
                   (let* ((move (car moves))
                          (state (board-make-move! b move))
                          ;; Negamax: negate child's score
                          (score (- (search b (- depth 1) (- beta) (- current-alpha)))))
                     (board-unmake-move! b move state)
                     (loop (cdr moves)
                           (max best-score score)
                           (if (> score best-score) move best-move)
                           (max current-alpha score))))))))))))
```

Checkmate is detected by an empty legal move list combined with the king being in check. The score `(- -30000 (- max-depth depth))` encodes "checkmate in N moves" — positions with faster checkmates score higher (closer to -30000) because `(- max-depth depth)` is smaller when the mate happens sooner. This allows the engine to prefer forced mates over equivalent material wins.

### Quiescence Search

The quiescence extension searches only captures (and promotions) past the nominal depth, continuing until the position is "quiet":

```scheme
(def (quiescence-search b alpha beta)
  (set! nodes-visited (+ nodes-visited 1))
  (let ((stand-pat (evaluate-board b)))
    (cond
      ;; Beta cutoff: opponent already has better
      ((>= stand-pat beta) beta)
      (else
       (let ((current-alpha (max alpha stand-pat)))
         ;; Generate and filter only legal captures/promotions
         (let* ((all-pseudo (get-pseudo-legal-moves b))
                (captures '()))
           (for-each
             (lambda (m)
               (when (or (> (move-struct-piece-captured m) EMPTY)
                         (> (move-struct-promotion m) EMPTY))
                 (let* ((state (board-make-move! b m))
                        (k-sq (vector-ref (board-king-square b)
                                          (color-idx (opponent (board-turn b))))))
                   (when (not (is-square-attacked? b k-sq (board-turn b)))
                     (set! captures (cons m captures)))
                   (board-unmake-move! b m state))))
             all-pseudo)
           ;; Recurse through sorted captures
           ...))))))
```

The **stand-pat** score is the static evaluation without making any further capture. If the stand-pat already beats beta, the position is returned immediately — the opponent would not have allowed reaching this node in the first place. This is the quiescence equivalent of alpha-beta pruning.

### Iterative Deepening

The top-level `get-best-move` function wraps the search in an iterative deepening loop:

```scheme
(def (get-best-move b (depth 3))
  (when (> (table-length transposition-table) 500000)
    (set! transposition-table (make-hash-table)))
  (let ((best-move #f) (best-score 0))
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
             ...)
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
        ;; Store root best move in TT for next iteration's ordering
        (when current-best
          (hash-put! transposition-table (board-zobrist-hash b)
                     (make-tt-entry d current-score TT_EXACT current-best)))))))
```

At each depth the transposition table best move from the previous depth is used to order the root moves. This "hash move" ordering is the single most effective pruning technique in practice.

## The CLI Module: cli.ss

The CLI module handles display and user input. It defines Unicode glyphs for each piece and renders the board using ANSI escape codes for colored squares.

```scheme
;; File: cli.ss
(import "engine" "ai" :std/format :std/pregexp :gerbil/gambit)

;; Unicode chess pieces
(def glyphs (make-vector 32 " "))
(vector-set! glyphs (logior WHITE KING)   "♔")
(vector-set! glyphs (logior WHITE QUEEN)  "♕")
(vector-set! glyphs (logior WHITE ROOK)   "♖")
(vector-set! glyphs (logior WHITE BISHOP) "♗")
(vector-set! glyphs (logior WHITE KNIGHT) "♘")
(vector-set! glyphs (logior WHITE PAWN)   "♙")
(vector-set! glyphs (logior BLACK KING)   "♚")
(vector-set! glyphs (logior BLACK QUEEN)  "♛")
(vector-set! glyphs (logior BLACK ROOK)   "♜")
(vector-set! glyphs (logior BLACK BISHOP) "♝")
(vector-set! glyphs (logior BLACK KNIGHT) "♞")
(vector-set! glyphs (logior BLACK PAWN)   "♟")
```

The Unicode piece codepoints (U+2654 through U+265F) are the standard chess symbols present in most modern terminal fonts. The vector is indexed by piece encoding (e.g., `logior WHITE KING` = 14), giving O(1) glyph lookup.

### Board Rendering

```scheme
(def (print-board b)
  (let ((info '()) (grid (board-grid b)))
    ;; Build sidebar information items
    (set! info (cons (format "\x1b;[1;37mTurn: ~a\x1b;[0m"
                             (if (= (board-turn b) WHITE) "White" "Black")) info))
    (set! info (cons (format "Eval: ~a"
                             (/ (round (/ (evaluate-board b) 10.0)) 10.0)) info))
    ;; ... more info items
    (do ((rank 7 (- rank 1)))
        ((< rank 0))
      (let ((row (list (format "\x1b;[90m~a\x1b;[0m " (+ rank 1)))))
        (do ((file 0 (+ file 1)))
            ((= file 8))
          (let* ((sq (+ (* rank 8) file))
                 (p (vector-ref grid sq))
                 ;; Alternate dark/light square background colors
                 (bg (if (= (modulo (+ rank file) 2) 0)
                       "\x1b;[48;5;237m"   ; dark grey
                       "\x1b;[48;5;94m"))) ; brown
            (if (= p EMPTY)
              (set! row (cons (string-append bg "   \x1b;[0m") row))
              (let ((color-code (if (= (piece-color p) WHITE)
                                  "\x1b;[1;37m"   ; bright white
                                  "\x1b;[1;35m"))) ; bright magenta
                (set! row (cons (format "~a~a ~a \x1b;[0m" bg color-code
                                        (vector-ref glyphs p)) row))))))
        (displayln (string-append (string-join (reverse row) "")
                                  "  " (list-ref info-list (- 7 rank))))))))
```

The checkerboard pattern is produced by checking whether `(rank + file)` is even or odd, selecting between ANSI 256-color codes for dark grey (`237`) and brown (`94`). Each square is three characters wide: a space, the piece glyph, and a space.

### Move Input and Game Loop

Moves are entered in UCI notation: the source square followed by the destination square, with an optional promotion character (e.g., `e2e4`, `e7e8q`). The `parse-move` function looks the UCI string up in the legal move list rather than constructing a move object directly, ensuring the engine only executes genuinely legal moves:

```scheme
(def (parse-move b input)
  (let* ((s (string-trim input)) (len (string-length s)))
    (if (or (< len 4) (> len 5))
      #f
      (let* ((from (hash-ref name-to-square (substring s 0 2) #f))
             (to   (hash-ref name-to-square (substring s 2 4) #f))
             (promo-char (if (= len 5) (string-ref s 4) #f))
             (promo-type (cond
                           ((not promo-char) EMPTY)
                           ((char=? promo-char #\q) QUEEN)
                           ...))
             (legal-moves (get-legal-moves b)))
        (let loop ((moves legal-moves))
          (cond
            ((null? moves) #f)
            (else
             (let* ((m (car moves))
                    (m-from (move-struct-from m))
                    (m-to (move-struct-to m))
                    (m-promo (move-struct-promotion m)))
               (if (and (= m-from from) (= m-to to))
                 ;; Match promotion type (default to queen)
                 (cond
                   ((and (> promo-type 0) (= m-promo promo-type)) m)
                   ((and (= promo-type 0) (= m-promo 0)) m)
                   (else (loop (cdr moves))))
                 (loop (cdr moves)))))))))))
```

The game loop handles three modes: human vs. bot, bot vs. human, and bot vs. bot. In bot-vs-bot mode a 500ms sleep between moves makes the game watchable at full speed. In-check detection, checkmate, stalemate, and the fifty-move rule are all tested before each move.

## The Perft Module: perft.ss

Perft (performance test) is the standard correctness benchmark for chess engines. It counts the number of leaf nodes reachable from a starting position at a given depth and compares the result against known-correct values. Any deviation indicates a bug in move generation.

```scheme
;; File: perft.ss
(import "engine")

(def (perft board depth)
  (if (= depth 0)
    1
    (let ((nodes 0)
          (moves (get-legal-moves board)))
      (for-each
        (lambda (move)
          (let* ((state (board-make-move! board move))
                 (recomputed (board-compute-zobrist-hash board)))
            ;; Verify Zobrist hash is consistent after every move
            (unless (= recomputed (board-zobrist-hash board))
              (error (format "Hash mismatch after ~a: stored=~a recomputed=~a"
                             (move->uci move) (board-zobrist-hash board) recomputed)))
            (set! nodes (+ nodes (perft board (- depth 1))))
            (board-unmake-move! board move state)
            ;; Verify hash is restored to pre-move value
            (unless (= (board-zobrist-hash board) (board-state-zobrist-hash state))
              (error (format "Hash not restored after unmake ~a" (move->uci move))))))
        moves)
      nodes)))
```

The perft function doubles as a Zobrist hash integrity checker: after every make/unmake pair it verifies that the incremental hash equals a freshly recomputed hash. This catches any bug in the incremental update logic immediately.

The test runner checks three depths against known values:

```scheme
(def (run-perft)
  (let ((board (new-board))
        (expected '#(20 400 8902)))
    (displayln "Perft tests from starting position:\n")
    (do ((depth 1 (+ depth 1)))
        ((> depth 3))
      (let* ((start (time->seconds (current-time)))
             (nodes (perft board depth))
             (elapsed (- end start))
             (exp (vector-ref expected (- depth 1)))
             (status (if (= nodes exp) "PASS" "FAIL")))
        (displayln (format "Depth ~a: ~a nodes (expected ~a) [~a] ~as (~a nps)"
                           depth nodes exp status elapsed nps))))))
```

The expected node counts (20, 400, 8902) are universally accepted reference values for the standard starting position. Depth 1 checks that exactly 20 opening moves are generated. Depth 2 verifies that each of those 20 positions generates exactly 20 responses (20 × 20 = 400). Depth 3 is where errors in promotion, en passant, and castling handling typically appear.

## Running the Code

All targets are available through the Makefile. Make sure Gerbil Scheme (`gxi` and `gxc`) is installed and on your `PATH`.

```
$ cat Makefile
.PHONY: run test compile clean

run:
	gxi cli.ss

test:
	gxi perft.ss

compile:
	gxc engine.ss ai.ss

clean:
	rm -rf .gerbil/
```

### Running the Perft Suite

```
$ make test
gxi perft.ss
Perft tests from starting position:

Depth 1: 20 nodes (expected 20) [PASS] 0.0s (42500 nps)
Depth 2: 400 nodes (expected 400) [PASS] 0.01s (58300 nps)
Depth 3: 8902 nodes (expected 8902) [PASS] 0.14s (63800 nps)
```

All three depths pass, confirming that the move generator produces exactly the correct number of positions and that the Zobrist hash remains consistent through every make/unmake pair. The nodes-per-second (nps) figure reflects the interpreted speed of `gxi`; compiling with `gxc` typically doubles or triples this figure.

### Running the Interactive Game

```
$ make run
gxi cli.ss

=== Chess Game ===

1. Play as White
2. Play as Black
3. Bot vs Bot

Choose mode (1-3): 1
Bot depth (1-6, default 3): 3

8  ♜  ♞  ♝  ♛  ♚  ♝  ♞  ♜   Turn: White
7  ♟  ♟  ♟  ♟  ♟  ♟  ♟  ♟   Move: 1
6                              50-move: 0
5                              EP: -
4                              Castling: KQkq
3                              Eval: 0.0
2  ♙  ♙  ♙  ♙  ♙  ♙  ♙  ♙
1  ♖  ♘  ♗  ♕  ♔  ♗  ♘  ♖
   a  b  c  d  e  f  g  h

Your move (or help): e2e4
...
Bot is thinking (depth 3)...
Bot plays: e7e5 | eval: 0.0 | nodes: 12481 | time: 0.8s
```

You enter moves in UCI notation. The sidebar updates after each move to show whose turn it is, the full-move number, the fifty-move counter, the en-passant target square, castling availability, and the static evaluation in pawn units.

Available in-game commands:

| Command | Description |
|---------|-------------|
| `e2e4` | Standard UCI move (source square + destination) |
| `e7e8q` | Pawn promotion (append `q`, `r`, `b`, or `n`) |
| `legal` | Print all legal moves in the current position |
| `fen` | Print the FEN string for the current position |
| `setfen` | Load a position from a FEN string |
| `reset` | Reset to the starting position |
| `help` | Show command list |
| `exit` | Quit the game |

### Bot vs. Bot Mode

Choosing mode 3 plays both sides automatically at the selected depth. This is useful for watching the engine's opening and middlegame tendencies:

```
Choose mode (1-3): 3
Bot depth (1-6, default 3): 4

Bot is thinking (depth 4)...
Bot plays: e2e4 | eval: 0.5 | nodes: 88420 | time: 1.2s

Bot is thinking (depth 4)...
Bot plays: e7e5 | eval: 0.0 | nodes: 92310 | time: 1.3s

Bot is thinking (depth 4)...
Bot plays: g1f3 | eval: 0.4 | nodes: 79810 | time: 1.1s
...
```

## Interpreting the Output

### Perft Node Counts

The perft numbers are not arbitrary. At depth 1, exactly 20 moves are legal from the starting position: 16 pawn moves (two squares or one square for each of the eight pawns) and 4 knight moves. At depth 2, all 20 of white's replies also have 20 legal responses (by symmetry of the starting position), giving 400. At depth 3, 8902 nodes are reached — the divergence from `20^3 = 8000` arises because of en-passant possibilities created by the depth-2 double pawn pushes, and because some positions at depth 2 allow more than 20 responses.

A perft FAIL at depth 1 almost always indicates an error in basic piece movement. A FAIL at depth 3 that passes depths 1 and 2 typically points to a bug in en-passant, castling, or promotion handling, since those edge cases first appear at depth 3.

### Evaluation Scores

The evaluation sidebar shows the position score in pawn units, from White's perspective. A score of `+0.5` means White has a slight material or positional advantage worth half a pawn. A score of `+3.0` corresponds to a full rook advantage. Scores above `+20.0` indicate a forced checkmate has been found (the engine uses `30000` as the base checkmate value, scaled by distance).

The `nodes` count in the bot output reflects how many positions the engine examined. At depth 3, a typical middlegame position has around 10,000–50,000 nodes. At depth 4, expect 100,000–500,000. The dramatic reduction compared to a full minimax tree (`~30^depth`) is the combined effect of alpha-beta pruning, move ordering, and the transposition table.

### Node Throughput

The `nps` figure from the perft run is a measure of raw engine speed in the Gerbil interpreter. Typical interpreted throughput is 50,000–100,000 nodes per second. Running `make compile` first to produce optimized Gambit bytecode usually doubles this to 100,000–200,000 nps, sufficient for a responsive depth-4 or depth-5 search in under two seconds per move.

## Wrap Up

In this chapter we built a complete chess engine and AI bot in Gerbil Scheme. Starting from first principles, we designed a mailbox board representation with bitwise piece encoding, precomputed move tables for knights, kings, and sliding pieces, and an incremental Zobrist hashing scheme that provides O(1) position identity checks. The legal move generator handles all of chess's special cases: pawn double pushes, en-passant captures, pawn promotions to any piece, and kingside and queenside castling with transit square attack checks.

On top of this engine we layered an AI search that combines iterative deepening negamax with alpha-beta pruning, a transposition table keyed on Zobrist hashes, MVV-LVA and PST-delta move ordering, and a quiescence extension that resolves captures before evaluating leaf nodes. The static evaluator combines material counts with piece-square tables that encode positional knowledge, switching between middlegame and endgame king tables based on remaining material.

The project demonstrates several Gerbil Scheme idioms worth carrying into other programs: aliasing long function names for readability, using `defstruct` with `transparent: #t` for easily printed structures, combining `do` loops for startup initialization with recursive `lambda` for runtime logic, and the make/unmake pattern for reversible state mutation that avoids copying large data structures during search.

The perft suite provides a disciplined correctness gate — a chess engine that passes all three depths is move-generation correct. The bot at depth 4 produces recognizable opening play (1. e4 e5 2. Nf3 followed by natural development) and will consistently defeat beginners, making this a satisfying demonstration of how much strategic behavior emerges from a relatively simple evaluation function combined with deep search.

A natural next step is to extend the search with **null move pruning** (trying a pass move to detect positions where the opponent has no effective threats) and **late move reductions** (searching the last few moves at a reduced depth), which together can double the effective search depth at no additional node cost.

## Practice Problems

{exercise, type: "coding"}

**Exercise 1: Display Board State as Unicode Art**

Write a Gerbil Scheme function `board->string` that returns the current board position as a multi-line string of Unicode chess glyphs with rank numbers and file letters, but without any ANSI color codes. The function should be suitable for logging or writing the board to a file. Use the `glyphs` vector from `cli.ss` for piece rendering.

Expected output for the starting position:

```
8 ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
6 . . . . . . . .
5 . . . . . . . .
4 . . . . . . . .
3 . . . . . . . .
2 ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
1 ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
  a b c d e f g h
```

{/exercise}

{exercise, type: "coding"}

**Exercise 2: Count Attacked Squares**

Using `is-square-attacked?` from `engine.ss`, write a function `count-attacked-squares` that takes a board `b` and a color (WHITE or BLACK) and returns the number of squares on the board that are attacked by that color. Run it on the starting position for both colors and verify that the results are equal (by symmetry).

{/exercise}

{exercise, type: "coding"}

**Exercise 3: Perft Divide**

Implement `perft-divide`, a variant of perft that prints, for each legal move from the root position, the number of leaf nodes reachable via that move at depth `d-1`. This "divide" output is the standard tool for isolating which move produces an incorrect node count. The starting position at depth 3 should show 20 moves, each with its node count, summing to 8902.

```
a2a3:  8457
a2a4:  9329
b2b3:  9345
...
Total: 8902
```

{/exercise}

{exercise, type: "coding"}

**Exercise 4: Material Balance Reporter**

Write a function `material-report` that takes a board and returns a formatted string breaking down the material count for each side by piece type. For example:

```
White: P×8=800  N×2=640  B×2=660  R×2=1000  Q×1=900  Total=4000
Black: P×8=800  N×2=640  B×2=660  R×2=1000  Q×1=900  Total=4000
Advantage: even
```

If one side is ahead, print the advantage in pawn units (e.g., `White +1.5`). Use this function to instrument the bot-vs-bot loop, printing the material report after every ten moves.

{/exercise}

{exercise, type: "coding"}

**Exercise 5: Opening Book Integration**

Create a simple opening book as a Gerbil hash table mapping FEN strings (of positions after 0, 1, and 2 moves) to a list of UCI move strings. Modify `get-best-move` in `ai.ss` to check the book first and, if the current position is in the book, pick a random move from the list rather than running the search. Seed the book with at least three openings (e.g., `1. e4`, `1. d4`, and `1. Nf3`) and their most common replies. Verify that the bot plays book moves from the starting position.

{/exercise}
