;; File: perft.ss
(import "engine")
(import :std/format
        :gerbil/gambit)

(def (perft board depth)
  (if (= depth 0)
    1
    (let ((nodes 0)
          (moves (get-legal-moves board)))
      (for-each
        (lambda (move)
          (let* ((state (board-make-move! board move))
                 (recomputed (board-compute-zobrist-hash board)))
            ;; Verify Zobrist hash consistency
            (unless (= recomputed (board-zobrist-hash board))
              (error (format "Hash mismatch after ~a: stored=~a recomputed=~a" 
                             (move->uci move) (board-zobrist-hash board) recomputed)))
            
            (set! nodes (+ nodes (perft board (- depth 1))))
            (board-unmake-move! board move state)
            
            ;; Verify hash restored
            (unless (= (board-zobrist-hash board) (board-state-zobrist-hash state))
              (error (format "Hash not restored after unmake ~a" (move->uci move))))))
        moves)
      nodes)))

(def (run-perft)
  (let ((board (new-board))
        (expected '#(20 400 8902)))
    (displayln "Perft tests from starting position:\n")
    (do ((depth 1 (+ depth 1)))
        ((> depth 3))
      (let* ((start (time->seconds (current-time)))
             (nodes (perft board depth))
             (end (time->seconds (current-time)))
             (elapsed (- end start))
             (exp (vector-ref expected (- depth 1)))
             (status (if (= nodes exp) "PASS" "FAIL"))
             (nps (if (> elapsed 0.0) (round (/ nodes elapsed)) 0)))
        (displayln (format "Depth ~a: ~a nodes (expected ~a) [~a] ~as (~a nps)"
                           depth nodes exp status (/ (round (* elapsed 100.0)) 100.0) nps))))))

(run-perft)
