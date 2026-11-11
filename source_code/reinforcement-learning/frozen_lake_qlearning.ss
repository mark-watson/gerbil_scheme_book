;; File: frozen_lake_qlearning.ss
(import :std/format
        :gerbil/gambit
        "../shared/format")

(export make-frozen-lake
        frozen-lake-reset!
        frozen-lake-step!
        frozen-lake-env
        frozen-lake-env?
        step-result
        step-result?
        step-result-next-state
        step-result-reward
        step-result-done
        make-q-table
        q-learning
        evaluate
        main)

;; Pad string left
(def (pad-left str len char)
  (let ((diff (- len (string-length str))))
    (if (> diff 0)
      (string-append (make-string diff char) str)
      str)))

;; Environment Setup
(defstruct frozen-lake-env
  (state slippery grid n-states n-actions)
  transparent: #t)

(defstruct step-result
  (next-state reward done)
  transparent: #t)

(def (make-frozen-lake (slippery #t))
  (let ((grid '#("S" "F" "F" "F" "F" "H" "F" "H" "F" "F" "F" "H" "H" "F" "F" "G")))
    (make-frozen-lake-env 0 slippery grid 16 4)))

(def (frozen-lake-reset! env)
  (set! (frozen-lake-env-state env) 0)
  0)

(def (frozen-lake-step! env action)
  (let* ((state (frozen-lake-env-state env))
         (slippery (frozen-lake-env-slippery env))
         (grid (frozen-lake-env-grid env))
         ;; If slippery, there's a 2/3 chance of slipping to an orthogonal direction
         (a (if (and slippery (< (random-real) 0.667))
                (modulo (+ action (if (< (random-real) 0.5) -1 1) 4) 4)
                action))
         (r (quotient state 4))
         (c (modulo state 4))
         ;; dirs: UP (0), RIGHT (1), DOWN (2), LEFT (3)
         (dirs '#((-1 0) (0 1) (1 0) (0 -1)))
         (dir (vector-ref dirs a))
         (dr (car dir))
         (dc (cadr dir))
         (nr (max 0 (min 3 (+ r dr))))
         (nc (max 0 (min 3 (+ c dc))))
         (next-state (+ (* nr 4) nc))
         (cell (vector-ref grid next-state))
         (reward (if (equal? cell "G") 1.0 0.0))
         (done (or (equal? cell "H") (equal? cell "G"))))
    (set! (frozen-lake-env-state env) next-state)
    (make-step-result next-state reward done)))

;; Q-Table Setup and Helpers
(def (make-q-table states actions)
  (let ((Q (make-vector states)))
    (do ((i 0 (+ i 1)))
        ((= i states) Q)
      (vector-set! Q i (make-vector actions 0.0)))))

(def (argmax vec)
  (let ((len (vector-length vec)))
    (let loop ((i 1) (best-idx 0) (best-val (vector-ref vec 0)))
      (if (= i len)
        best-idx
        (let ((val (vector-ref vec i)))
          (if (> val best-val)
            (loop (+ i 1) i val)
            (loop (+ i 1) best-idx best-val)))))))

(def (vector-max vec)
  (let ((len (vector-length vec)))
    (let loop ((i 1) (best-val (vector-ref vec 0)))
      (if (= i len)
        best-val
        (loop (+ i 1) (max best-val (vector-ref vec i)))))))

;; Evaluation
(def (evaluate env Q (episodes 100))
  (let ((wins 0))
    (do ((i 0 (+ i 1)))
        ((= i episodes) (/ wins (exact->inexact episodes)))
      (frozen-lake-reset! env)
      (let loop ((s 0) (steps 0))
        (let* ((a (argmax (vector-ref Q s)))
               (step-res (frozen-lake-step! env a))
               (next-state (step-result-next-state step-res))
               (reward (step-result-reward step-res))
               (done (step-result-done step-res)))
          (cond
            ((and done (= reward 1.0))
             (set! wins (+ wins 1)))
            ((and (not done) (< steps 100))
             (loop next-state (+ steps 1)))))))))

;; Q-learning Agent
(def (q-learning env (episodes 10000) (alpha 0.1) (gamma 0.99) (eps 1.0) (decay 0.999) (min-eps 0.01))
  (let* ((n-states (frozen-lake-env-n-states env))
         (n-actions (frozen-lake-env-n-actions env))
         (Q (make-q-table n-states n-actions))
         (current-eps eps))
    (do ((ep 0 (+ ep 1)))
        ((= ep episodes) Q)
      (let ((s (frozen-lake-reset! env)))
        (let loop ((s s) (steps 0))
          (let* ((a (if (< (random-real) current-eps)
                      (random-integer n-actions)
                      (argmax (vector-ref Q s))))
                 (step-res (frozen-lake-step! env a))
                 (next-state (step-result-next-state step-res))
                 (reward (step-result-reward step-res))
                 (done (step-result-done step-res))
                 (s-q (vector-ref Q s))
                 (old-val (vector-ref s-q a))
                 (next-max (vector-max (vector-ref Q next-state)))
                 (new-val (+ old-val (* alpha (- (+ reward (* gamma next-max)) old-val)))))
            (vector-set! s-q a new-val)
            (when (and (not done) (< steps 100))
              (loop next-state (+ steps 1))))))
      (set! current-eps (max min-eps (* current-eps decay)))
      (when (= (modulo (+ ep 1) 1000) 0)
        (displayln (format "  Episode ~a: success = ~a"
                           (pad-left (number->string (+ ep 1)) 5 #\space)
                           (format-decimal (evaluate env Q) 2)))))))

(def (main)
  (let ((env (make-frozen-lake #t)))
    (displayln "=== Q-Learning on FrozenLake ===")
    (displayln (format "States: ~a, Actions: ~a" (frozen-lake-env-n-states env) (frozen-lake-env-n-actions env)))
    (displayln "")
    (displayln "Training:")
    (let* ((Q (q-learning env))
           (arrows '#("↑" "→" "↓" "←")))
      (displayln "")
      (displayln "Learned policy:")
      (do ((r 0 (+ r 1)))
          ((= r 4))
        (let ((line "  "))
          (do ((c 0 (+ c 1)))
              ((= c 4))
            (let* ((s (+ (* r 4) c))
                   (best-a (argmax (vector-ref Q s)))
                   (arrow (vector-ref arrows best-a)))
              (set! line (string-append line arrow))))
          (displayln line)))
      
      (displayln "")
      (displayln (format "Final success rate (1000 episodes): ~a"
                         (format-decimal (evaluate env Q 1000) 2))))))

(export main)
