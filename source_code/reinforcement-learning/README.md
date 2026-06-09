# Reinforcement Learning Examples

A collection of core reinforcement learning algorithms implemented from scratch in Gerbil Scheme. These examples are ported from TypeScript.

## Examples

### 1. Q-Learning on Frozen Lake (`frozen_lake_qlearning.ss`)

This example implements Q-learning on a grid-based environment (the standard 4×4 FrozenLake grid).
- **Environment:** 16 states (cells), 4 actions (UP, RIGHT, DOWN, LEFT). The goal is to traverse from Start (S) to Goal (G) on a slippery frozen lake without falling into Holes (H).
- **Agent:** Learns a tabular Q-function using temporal difference learning with $\epsilon$-greedy action selection and exploration decay.
- **Output:** Outputs evaluation metrics during training, displays the learned optimal policy grid using arrows (↑, →, ↓, ←), and prints the final success rate.

### 2. MDP Value Iteration (`mdp_demo.ss`)

This example demonstrates Value Iteration on a Markov Decision Process (MDP) for a 3×3 Grid World.
- **State Space:** 9 grid positions.
- **Transition Dynamics:** Deterministic movements in 4 directions, bouncing off borders back into the same cell.
- **Rewards:** Goal state at cell 8 has a reward of +10, and a trap state at cell 5 has a penalty of -5.
- **Output:** Calculates the optimal state-value function $V^*$ and outputs the resulting policy arrows for the grid.

## How to Run

Use the provided `Makefile` to run the examples using the `gxi` interpreter:

```bash
# Run Q-Learning on FrozenLake
make run-qlearning

# Run MDP Value Iteration
make run-mdp
```
