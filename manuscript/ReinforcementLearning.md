# Reinforcement Learning: Value Iteration and Q-Learning

Reinforcement learning (RL) is the branch of machine learning that studies how an agent should act in an environment in order to maximize a cumulative reward signal. Unlike supervised learning, there is no labeled dataset saying "here is the right action for this input". Instead the agent must discover, through trial and error, which sequences of actions eventually pay off. This framing captures an enormous range of practical problems: robots learning to walk, programs learning to play board games, recommender systems learning what content to surface, and adaptive controllers for chemical plants, elevators, and datacenter cooling systems.

In this chapter we implement two classical RL algorithms in Gerbil Scheme, both from scratch and with no external dependencies. The first, **Value Iteration**, solves a fully known Markov Decision Process by directly computing the optimal value of every state. The second, **Q-Learning**, drops the assumption that the transition model is known and learns action values by simply interacting with an environment. Value Iteration and Q-Learning are the two canonical algorithms in the field, and they illustrate the two very different worlds RL practitioners work in: model-based planning versus model-free learning.

## Theoretical Background

### Markov Decision Processes

A **Markov Decision Process** (MDP) is the standard mathematical framework for RL. An MDP consists of:

- A finite set of states `\mathcal{S}`$.
- A finite set of actions `\mathcal{A}`$.
- A transition function `P(s' \mid s, a)`$ giving the probability of ending in state `s'`$ after taking action `a`$ in state `s`$.
- A reward function `R(s, a)`$ giving the expected immediate reward for taking action `a`$ in state `s`$.
- A discount factor `\gamma \in [0, 1)`$ that weighs immediate rewards more heavily than distant ones.

The **Markov property** is that the next state depends only on the current state and action, not on the history that led to the current state. This assumption is what makes the problem tractable, and it is a surprisingly good approximation in many domains.

A **policy** `\pi`$ is a rule that tells the agent which action to take in each state. The value of following policy `\pi`$ starting in state `s`$ is the expected discounted return:

```$
V^{\pi}(s) = \mathbb{E}\left[\sum_{t=0}^{\infty} \gamma^t R(s_t, a_t) \;\Big|\; s_0 = s, \; \pi\right]
```

The goal of RL is to find the **optimal policy** `\pi^*`$ that maximizes this expected return from every state.

### The Bellman Optimality Equation

The optimal value function `V^*`$ satisfies the recursive Bellman optimality equation:

```$
V^*(s) = \max_{a} \left[ R(s, a) + \gamma \sum_{s'} P(s' \mid s, a) V^*(s') \right]
```

This equation says: the value of the best action from state `s`$ equals the immediate reward plus the discounted expected value of where you end up. Once we know `V^*`$, the optimal policy is trivially derived by acting greedily with respect to it:

```$
\pi^*(s) = \operatorname*{arg\,max}_{a} \left[ R(s, a) + \gamma \sum_{s'} P(s' \mid s, a) V^*(s') \right]
```

### Value Iteration

**Value Iteration** solves the Bellman equation by turning it into an update rule. Starting from `V_0(s) = 0`$ everywhere, we repeatedly apply:

```$
V_{k+1}(s) \leftarrow \max_{a} \left[ R(s, a) + \gamma \sum_{s'} P(s' \mid s, a) V_k(s') \right]
```

This contraction converges to `V^*`$ for any `\gamma < 1`$. In practice we stop when the maximum change across all states over one sweep drops below a small threshold.

Value Iteration requires the full transition and reward model of the environment. When we have that model, it is elegant and exact.

### Q-Learning

In many real problems we do not know `P`$ and `R`$. The agent has to learn purely from experience. This is what **Q-Learning** does. Instead of storing a value per state, we store a **Q-value** `Q(s, a)`$ for every state-action pair. Given a transition `(s, a, r, s')`$ observed during play, we update:

```$
Q(s, a) \leftarrow Q(s, a) + \alpha \left[ r + \gamma \max_{a'} Q(s', a') - Q(s, a) \right]
```

The bracketed quantity is called the **temporal-difference (TD) error**. It is the discrepancy between our current estimate of `Q(s, a)`$ and the newly observed one-step lookahead. The learning rate `\alpha \in (0, 1]`$ controls how aggressively we pull our estimate toward the new evidence.

### Exploration versus Exploitation

If the agent always picks the action it currently thinks is best, it will never discover superior alternatives. To break out of this trap, Q-Learning uses an **`\epsilon`$-greedy** strategy: with probability `\epsilon`$ take a random action (exploration), otherwise take the current best action (exploitation). Over the course of training we anneal `\epsilon`$ from a large value (say 1.0) down to a small floor (say 0.01), so the agent explores aggressively early and exploits its learned knowledge later.

## Project Structure

The project directory `source_code/reinforcement-learning` contains:

| File | Description |
|------|-------------|
| `mdp_demo.ss` | Value Iteration on a deterministic `3x3` grid world. |
| `frozen_lake_qlearning.ss` | Q-Learning on the stochastic `4x4` FrozenLake environment. |
| `Makefile` | Two targets: `run-mdp` and `run-qlearning`. |
| `gerbil.pkg` | Package declaration. |

Each Scheme file is self-contained: no shared modules, no external data files, and no dependencies beyond Gerbil's standard library.

## Example 1: Value Iteration on a 3×3 Grid World

The first example plants an agent in a `3 \times 3`$ grid with a rewarding goal cell and a punishing trap. The environment is deterministic: attempting to move up in cell 4 always lands in cell 1. Attempting to walk into a wall keeps the agent in place.

### Grid Layout

We number the cells row-major from `0`$ to `8`$:

```
+---+---+---+
| 0 | 1 | 2 |
+---+---+---+
| 3 | 4 | 5 |   <- cell 5 is a trap, R = -5
+---+---+---+
| 6 | 7 | 8 |   <- cell 8 is the goal, R = +10
+---+---+---+
```

The rewards are attached to actions taken **from** these cells (rather than to arriving there), so any action from cell 8 pays out +10 and any action from cell 5 pays out -5.

### Imports and Formatting Helpers

The file begins with imports and two small helpers for pretty-printing floating-point numbers. Gerbil's default float printing produces long decimal expansions (like `65.6100000000...`), which is inconvenient for reporting value functions:

```scheme
;; File: mdp_demo.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

;; Find index of character in string (helper for format-decimal)
(def (string-index str char)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) char) i)
        (else (loop (+ i 1)))))))

;; Format floating point numbers to a fixed number of decimal places
(def (format-decimal x places)
  (let* ((multiplier (expt 10 places))
         (rounded (/ (round (* x multiplier)) multiplier))
         (str (number->string rounded)))
    (let ((dot-pos (string-index str #\.)))
      (let ((formatted-str
             (if dot-pos
               (let* ((frac-len (- (string-length str) dot-pos 1))
                      (diff (- places frac-len)))
                 (if (> diff 0)
                   (string-append str (make-string diff #\0))
                   str))
               (string-append str "." (make-string places #\0)))))
        (if (char=? (string-ref formatted-str 0) #\.)
          (string-append "0" formatted-str)
          formatted-str)))))
```

### A 3D Vector Helper

The transition model `P`$ is naturally represented as a 3-dimensional array indexed by action, source state, and next state. Gerbil vectors are one-dimensional, so we build a 3D vector by nesting:

```scheme
;; Multi-dimensional vector creation helper
(def (make-3d-vector d1 d2 d3 (val 0.0))
  (let ((v1 (make-vector d1)))
    (do ((i 0 (+ i 1)))
        ((= i d1) v1)
      (let ((v2 (make-vector d2)))
        (do ((j 0 (+ j 1)))
            ((= j d2))
          (vector-set! v2 j (make-vector d3 val)))
        (vector-set! v1 i v2)))))
```

Reading a single probability out of `P`$ then requires three `vector-ref` calls: `(vector-ref (vector-ref (vector-ref P a) s) sp)`.

### The Value Iteration Loop

The core algorithm is a direct translation of the Bellman update. We sweep over every state, compute the value of each action as the immediate reward plus the discounted expected value of successor states, keep the best action, and remember both the new value and the greedy policy:

```scheme
;; Value Iteration Algorithm
(def (value-iteration nS nA P R (gamma 0.9) (threshold 1e-6))
  (let ((V (make-vector nS 0.0))
        (policy (make-vector nS 0))
        (iterations 0))
    (let loop ()
      (set! iterations (+ iterations 1))
      (let ((new-V (make-vector nS 0.0))
            (max-delta 0.0))
        (do ((s 0 (+ s 1)))
            ((= s nS))
          (let ((best-val -1.0e9)
                (best-a 0))
            (do ((a 0 (+ a 1)))
                ((= a nA))
              (let ((val (vector-ref (vector-ref R s) a)))
                (do ((sp 0 (+ sp 1)))
                    ((= sp nS))
                  (let ((prob (vector-ref (vector-ref (vector-ref P a) s) sp)))
                    (set! val (+ val (* gamma prob (vector-ref V sp))))))
                (when (> val best-val)
                  (set! best-val val)
                  (set! best-a a))))
            (vector-set! new-V s best-val)
            (vector-set! policy s best-a)
            (set! max-delta (max max-delta (abs (- best-val (vector-ref V s)))))))
        (set! V new-V)
        (if (< max-delta threshold)
          `((policy . ,policy)
            (V . ,V)
            (iterations . ,iterations))
          (loop))))))
```

The termination criterion `max-delta < threshold` is the numerical realization of "the values have stopped changing".

### Building the Transition and Reward Tables

The `main` function wires up the grid dynamics. The four actions are UP, RIGHT, DOWN, and LEFT, encoded as `0`$, `1`$, `2`$, `3`$. Given a state `s`$ we compute its row and column, apply the direction, clip to the grid boundary (bounce), and set the transition probability to 1.0:

```scheme
(def (main)
  (let* ((nS 9)
         (nA 4)
         ;; transition probability matrix P[nA][nS][nS]
         (P (make-3d-vector nA nS nS 0.0))
         ;; dirs: UP, RIGHT, DOWN, LEFT
         (dirs '#((-1 0) (0 1) (1 0) (0 -1))))
    
    ;; Set up grid transitions
    (do ((s 0 (+ s 1)))
        ((= s nS))
      (let* ((r (quotient s 3))
             (c (modulo s 3)))
        (do ((a 0 (+ a 1)))
            ((= a nA))
          (let* ((dir (vector-ref dirs a))
                 (dr (car dir))
                 (dc (cadr dir))
                 (nr (+ r dr))
                 (nc (+ c dc))
                 (next-s (if (and (>= nr 0) (< nr 3) (>= nc 0) (< nc 3))
                           (+ (* nr 3) nc)
                           s)))
            (vector-set! (vector-ref (vector-ref P a) s) next-s 1.0)))))
```

The reward matrix `R`$ is initialized to zero everywhere and then patched: any action from the goal cell 8 gives +10, and any action from the trap cell 5 gives -5:

```scheme
    ;; Set up reward matrix R[nS][nA]
    (let ((R (make-vector nS)))
      (do ((s 0 (+ s 1)))
          ((= s nS))
        (vector-set! R s (make-vector nA 0.0)))
      ;; Set up specific target rewards
      (do ((a 0 (+ a 1)))
          ((= a nA))
        (vector-set! (vector-ref R 8) a 10.0)
        (vector-set! (vector-ref R 5) a -5.0))
      
      ;; Run Value Iteration
      (let* ((res (value-iteration nS nA P R 0.9))
             (policy (cdr (assoc 'policy res)))
             (V (cdr (assoc 'V res)))
             (iterations (cdr (assoc 'iterations res)))
             (arrows '#("↑" "→" "↓" "←")))
        (displayln "=== Custom 3×3 Grid World ===")
        (displayln "Optimal policy:")
        (do ((r 0 (+ r 1)))
            ((= r 3))
          (let ((line ""))
            (do ((c 0 (+ c 1)))
                ((= c 3))
              (let* ((idx (+ (* r 3) c))
                     (a (vector-ref policy idx))
                     (arrow (vector-ref arrows a)))
                (set! line (string-append line "  " arrow "  "))))
            (displayln line)))
        
        (displayln "")
        (display "Value function: [")
        (do ((s 0 (+ s 1)))
            ((= s nS))
          (display (format-decimal (vector-ref V s) 2))
          (when (< s (- nS 1)) (display ", ")))
        (displayln "]")
        (displayln "Iterations: " iterations)))))

(main)
```

### Running the Value Iteration Demo

Run either `make run-mdp` or invoke the interpreter directly:

```console
$ make run-mdp
gxi mdp_demo.ss
=== Custom 3×3 Grid World ===
Optimal policy:
  →    ↓    ↓  
  →    ↓    ↓  
  →    →    →  

Value function: [65.61, 72.90, 76.50, 72.90, 81.00, 85.00, 81.00, 90.00, 100.00]
Iterations: 154
```

### Interpreting the Value Iteration Results

- **Optimal policy arrows** show the greedy action from every cell. From the top-left corner (cell 0) the best action is RIGHT, which begins the path 0 -> 1 -> 2 -> 5 -> 8. Even though cell 5 is a trap, the policy is willing to accept the -5 penalty once because reaching the goal is worth so much more.
- **Value function**: cell 8 has value 100.0, which is the goal reward of 10 amplified through the loop where every action from cell 8 collects another +10 discounted by `\gamma = 0.9`$. The infinite geometric series `10 + 9 + 8.1 + \dots`$ sums to exactly `10 / (1 - 0.9) = 100`$.
- **Value monotonicity**: values decrease with distance from the goal, from 100.0 at cell 8 down to 65.61 at cell 0. Each step away from the goal multiplies the payoff by `\gamma`$.
- **Cell 5 = 85.0**: even though cell 5 is a trap, its value is still positive and high, because from cell 5 you can immediately move DOWN into the goal at cell 8. The trap penalty is paid once, but the goal payoff dominates.
- **154 iterations**: the algorithm converged in 154 sweeps to a maximum change of less than `10^{-6}`$. The convergence rate is governed by `\gamma`$: a larger `\gamma`$ means longer-horizon planning and therefore more iterations before things settle.

## Example 2: Q-Learning on FrozenLake

The second example is much more interesting because the agent no longer knows the environment ahead of time and the environment itself is stochastic. **FrozenLake** is a `4 \times 4`$ grid modelled after a slippery frozen pond:

```
S F F F
F H F H
F F F H
H F F G
```

- `S` is the start (cell 0).
- `G` is the goal (cell 15). Stepping onto `G` yields a reward of +1 and ends the episode.
- `H` cells are holes. Stepping into `H` ends the episode with reward 0.
- `F` are frozen cells that are safe to walk on.

The twist is that the ice is slippery. With probability `\tfrac{2}{3}`$ the agent slips and moves in one of the two orthogonal directions instead of the one it chose. This means the agent cannot deterministically follow any path; it has to learn a policy that is robust to slipping.

### Building the Environment

The environment holds its state in a Gerbil hash table:

```scheme
;; File: frozen_lake_qlearning.ss
(import :std/format
        :std/pregexp
        :gerbil/gambit)

;; ... string-index and format-decimal helpers ...

;; Pad string left
(def (pad-left str len char)
  (let ((diff (- len (string-length str))))
    (if (> diff 0)
      (string-append (make-string diff char) str)
      str)))

;; Environment Setup
(def (make-frozen-lake (slippery #t))
  (let ((state 0)
        (grid '#("S" "F" "F" "F" "F" "H" "F" "H" "F" "F" "F" "H" "H" "F" "F" "G")))
    (list->hash-table
      `((state . ,state)
        (slippery . ,slippery)
        (grid . ,grid)
        (n-states . 16)
        (n-actions . 4)))))

(def (frozen-lake-reset! env)
  (hash-put! env 'state 0)
  0)
```

`frozen-lake-step!` implements the slippery dynamics. If `slippery` is true, it flips a coin: with probability `\tfrac{2}{3}`$ the requested action `a`$ is replaced by `(a - 1) \bmod 4`$ or `(a + 1) \bmod 4`$ (a `\pm 90`$-degree slip). Otherwise the action goes through as intended:

```scheme
(def (frozen-lake-step! env action)
  (let* ((state (hash-ref env 'state))
         (slippery (hash-ref env 'slippery))
         (grid (hash-ref env 'grid))
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
    (hash-put! env 'state next-state)
    `((next-state . ,next-state)
      (reward . ,reward)
      (done . ,done))))
```

The step function returns an association list with three keys: the new state, the reward received, and a boolean indicating whether the episode is over.

### The Q-Table

The Q-table is a vector of vectors: 16 states, each with a 4-element vector of Q-values (one per action). Two small helpers, `argmax` and `vector-max`, extract the greedy action and best Q-value for a state:

```scheme
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
```

### Evaluating the Current Policy

To measure progress during training we need a way to run the greedy policy that Q currently implies, without any exploration noise, and count how often it reaches the goal. `evaluate` runs a fixed number of episodes (default 100) using pure `argmax` action selection and returns the fraction of episodes that ended in success:

```scheme
;; Evaluation
(def (evaluate env Q (episodes 100))
  (let ((wins 0))
    (do ((i 0 (+ i 1)))
        ((= i episodes) (/ wins (exact->inexact episodes)))
      (frozen-lake-reset! env)
      (let loop ((s 0) (steps 0))
        (let* ((a (argmax (vector-ref Q s)))
               (step-res (frozen-lake-step! env a))
               (next-state (cdr (assoc 'next-state step-res)))
               (reward (cdr (assoc 'reward step-res)))
               (done (cdr (assoc 'done step-res))))
          (cond
            ((and done (= reward 1.0))
             (set! wins (+ wins 1)))
            ((and (not done) (< steps 100))
             (loop next-state (+ steps 1)))))))))
```

Note the 100-step cap. Without it, a very bad or oscillating policy could loop forever on the ice without ever hitting a hole or the goal.

### The Q-Learning Training Loop

Here is the heart of the algorithm. For each of 10,000 episodes we reset the environment and take actions until we finish. Each action is chosen either randomly (with probability `\epsilon`$) or greedily. After each step we update the Q-value for the chosen state-action pair using the temporal-difference rule:

```scheme
;; Q-learning Agent
(def (q-learning env (episodes 10000) (alpha 0.1) (gamma 0.99) (eps 1.0) (decay 0.999) (min-eps 0.01))
  (let ((Q (make-q-table 16 4))
        (current-eps eps))
    (do ((ep 0 (+ ep 1)))
        ((= ep episodes) Q)
      (let ((s (frozen-lake-reset! env)))
        (let loop ((s s) (steps 0))
          (let* ((a (if (< (random-real) current-eps)
                      (random-integer 4)
                      (argmax (vector-ref Q s))))
                 (step-res (frozen-lake-step! env a))
                 (next-state (cdr (assoc 'next-state step-res)))
                 (reward (cdr (assoc 'reward step-res)))
                 (done (cdr (assoc 'done step-res)))
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
```

Every 1000 episodes we run the evaluator so the reader can watch the policy improve. After each episode `\epsilon`$ is multiplied by `0.999`$, so after 10,000 episodes it has decayed from `1.0`$ toward the floor of `0.01`$.

### The Main Function

`main` wires everything together, prints the environment summary, trains the Q-table, then displays the learned greedy policy and a final held-out evaluation over 1000 episodes:

```scheme
(def (main)
  (let ((env (make-frozen-lake #t)))
    (displayln "=== Q-Learning on FrozenLake ===")
    (displayln (format "States: ~a, Actions: ~a" (hash-ref env 'n-states) (hash-ref env 'n-actions)))
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

(main)
```

### Running the Q-Learning Demo

Because Q-learning involves random exploration and a stochastic environment, the exact numbers change on every run. A representative session looks like this:

```console
$ make run-qlearning
gxi frozen_lake_qlearning.ss
=== Q-Learning on FrozenLake ===
States: 16, Actions: 4

Training:
  Episode  1000: success = 0.72
  Episode  2000: success = 0.61
  Episode  3000: success = 0.77
  Episode  4000: success = 0.74
  Episode  5000: success = 0.73
  Episode  6000: success = 0.44
  Episode  7000: success = 0.70
  Episode  8000: success = 0.76
  Episode  9000: success = 0.71
  Episode 10000: success = 0.71

Learned policy:
  ←↑↑↑
  ←↑←↑
  ↑↓←↑
  ↑→↓↑

Final success rate (1000 episodes): 0.79
```

### Interpreting the Q-Learning Results

The most immediately surprising thing about the learned policy is that it does not look at all like "walk toward the goal". Many cells appear to point *away* from the goal. This is the fingerprint of a policy that has learned to exploit the slipperiness of the ice.

Consider cell 0 (top-left), where the policy says LEFT. The agent obviously cannot walk further left than the wall. What actually happens is that pressing LEFT with `\tfrac{2}{3}`$ slip probability yields:

- `\tfrac{1}{3}`$ chance to actually go LEFT (stay in cell 0).
- `\tfrac{1}{3}`$ chance to slip UP (also stay in cell 0).
- `\tfrac{1}{3}`$ chance to slip DOWN (move to cell 4, a safe frozen cell).

The nominal "LEFT" action is really "make progress downward without ever risking a slip into the deadly hole at cell 5". The agent has learned to use the wall to filter out dangerous slips. This is a classic and beautiful phenomenon in stochastic RL: the optimal action often has to be chosen for its distribution of outcomes rather than its intended direction.

Other observations:

- **Success rates oscillate during training**. Because `\epsilon`$ is still positive, exploration occasionally overwrites good Q-values with noisy ones. This is unavoidable in tabular Q-learning without more advanced tricks like target networks or Double Q-learning.
- **Final success rate around 0.75–0.80** is typical for this environment and this algorithm. The theoretical maximum success rate on slippery FrozenLake with a Q-learning agent is a bit above 0.80. Getting significantly higher requires either less slipperiness or more sophisticated algorithms.
- **The learned policy is not unique**. Repeated runs produce different arrangements of arrows, all of which are approximately optimal. Any two policies that agree on the "moves that trigger the right slip distribution" have roughly equal expected return.

## Wrap Up

Value Iteration and Q-Learning are the two founding algorithms of reinforcement learning, and both fit comfortably into a few hundred lines of readable Gerbil Scheme. The exercise reveals several ideas that generalize far beyond these toy examples:

- **Bellman equations turn planning into a fixed-point computation**. Whenever you can write a recursive characterization of the value function, you can build a solver by iterating it.
- **Model-free learning is simply model-based planning with a moving estimate**. The Q-learning update rule is the Bellman update rule with the transition and reward model replaced by one sample of experience.
- **Exploration is not a nuisance to be minimized, it is a first-class part of the algorithm**. `\epsilon`$-greedy decay is a specific choice; entire subfields of RL exist to design better exploration strategies.
- **Stochastic dynamics reward counterintuitive policies**. Learning "walk away from the goal" is often correct on FrozenLake, because it neutralizes the slipperiness.

Gerbil Scheme's combination of vectors, hash tables, and named `let` loops makes it a natural language for these numerical experiments. The imperative flavor of the code, with `set!` and `vector-set!`, may feel unusual to Schemers coming from a purely functional background, but it maps directly onto how these algorithms are described in textbooks and helps keep the code close to the theory.

## Optional Practice Problems

1. **Deterministic FrozenLake**: Call `(make-frozen-lake #f)` instead of `(make-frozen-lake #t)` and rerun `q-learning`. Report the final success rate and describe how the learned policy differs from the slippery case. Explain why the arrows now point unambiguously toward the goal.

2. **Sweep the Discount Factor**: In `mdp_demo.ss`, run `value-iteration` with `gamma` set to `0.5`, `0.7`, `0.9`, and `0.99`. Report the number of iterations to convergence and the value at cell 0 for each `\gamma`$. Explain the relationship between `\gamma`$ and convergence rate.

3. **Slower Epsilon Decay**: Change the `decay` argument in `q-learning` from `0.999` to `0.9995` and rerun. Does the final success rate improve? Does the training take noticeably longer to reach a stable policy? Explain how the exploration schedule affects sample efficiency.

4. **Add a Reward Shaper**: Modify `frozen-lake-step!` so that stepping onto a hole yields reward `-1.0` instead of `0.0`. Re-train and report how the learned policy changes. Discuss the tradeoffs of reward shaping versus letting the agent learn purely from the terminal +1 signal.

5. **Larger Grid World**: Generalize `mdp_demo.ss` from `3 \times 3`$ to `5 \times 5`$. Place a goal at the bottom-right corner, a trap somewhere in the interior, and run value iteration. Print the value function as a heat map (using formatted numbers in a grid layout) and the policy arrows side by side.

6. **Q-Learning on the 3×3 Grid**: Port the deterministic 3×3 grid world from `mdp_demo.ss` into a Q-learning environment analogous to FrozenLake and train a Q-table on it. Compare the learned policy and value estimates against the exact solution from Value Iteration. This exercise directly demonstrates the connection between model-free learning and model-based planning.

7. **Double Q-Learning**: Q-learning is known to suffer from **maximization bias**: the `\max_{a'} Q(s', a')`$ term systematically overestimates true action values. Implement Double Q-Learning, which maintains two independent Q-tables `Q_A`$ and `Q_B`$ and uses one to select the greedy action and the other to evaluate it. Compare final success rates against vanilla Q-learning across ten training runs.

8. **SARSA**: Implement SARSA, the on-policy sibling of Q-learning. Instead of updating toward `\max_{a'} Q(s', a')`$, SARSA updates toward `Q(s', a')`$ where `a'`$ is the action actually taken by the current `\epsilon`$-greedy policy. Compare SARSA's learned policy with Q-learning's on slippery FrozenLake and explain any qualitative differences (SARSA is typically more conservative near cliffs and holes).
