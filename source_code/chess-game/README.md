# Chess Engine & AI Bot in Gerbil Scheme

A complete, high-performance chess engine and AI bot implemented in Gerbil Scheme. This project is ported from a reference TypeScript implementation.

## Features

1. **Chess Engine (`engine.ss`):**
   - 64-element board representation with active piece list tracking.
   - Precomputed sliding ray and knight/king move tables.
   - Fully legal move generation including pawn double-push, captures, promotions, en-passant, and castling rights validation.
   - Incremental 64-bit Zobrist hashing for side-to-move, castling rights, en-passant file, and active pieces.
   - Fast state-restoring undo/redo mechanism.
   - FEN parsing and serialization.

2. **Search and AI Bot (`ai.ss`):**
   - Transposition table (caching depths, scores, bounds, and best moves).
   - Negamax search with alpha-beta pruning and iterative deepening.
   - Quiescence search to avoid the horizon effect on capture/promotion tactics.
   - Piece-square tables (PSTs) and endgame material detection for dynamic positional evaluations.

3. **Interactive Command-Line Interface (`cli.ss`):**
   - Mode selection (Play as White, Play as Black, Bot vs Bot).
   - Custom checkered terminal board rendering with ANSI colors and Unicode chess pieces.
   - Support for UCI moves (e.g. `e2e4`), and administrative commands like `fen`, `setfen`, `legal`, `reset`, `help`, and `exit`.

4. **Correctness Verification (`perft.ss`):**
   - Correctness and speed tests matching standard chess benchmarks:
     - Depth 1: 20 nodes
     - Depth 2: 400 nodes
     - Depth 3: 8902 nodes
   - Verifies incremental Zobrist hash invariants at every move and undo action.

## Getting Started

### Prerequisites

- [Gerbil Scheme](https://cons.io/) (`gxi` and `gxc` command-line tools installed and on your PATH).

### Running the Interactive Game

Launch the game in your terminal using the Makefile:

```bash
make run
```

### Running the Correctness & Performance Tests

Run the perft suite to verify move generation and Zobrist hash consistency:

```bash
make test
```

### Compiling the Source Code

To compile the chess engine and AI modules for faster performance:

```bash
make compile
```

## Command Line Interface Commands

During a game (when it is your turn), you can enter standard UCI moves (e.g., `e2e4`, `g8f6`) or one of the following commands:
- `help`: Print the list of available commands.
- `fen`: Print the current board's FEN string.
- `setfen`: Prompt to enter a custom FEN to set the board state.
- `legal`: Print all legal UCI moves in the current position.
- `reset`: Reset the board to the standard starting position.
- `exit` or `quit`: Terminate the chess application.
