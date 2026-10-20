# AI-Driven Text Adventure Game in Gerbil Scheme

An interactive, AI-driven text adventure game powered by a local **Ollama** model via its multi-turn `/api/chat` endpoint. Ported to Gerbil Scheme from the Common Lisp version in *Loving Common Lisp*.

The system prompt (`story.txt`) configures the model as a text adventure game master: it describes sensory details, introduces twists and NPCs, tracks status (health, inventory, progress), and presents branching choices to the player.

## Files

| File | Description |
|------|-------------|
| `game.ss` | Interactive game loop and Ollama `/api/chat` client |
| `story.txt` | Game master system prompt defining rules and setting |
| `test.ss` | Non-interactive smoke test simulating 2 full turns with Ollama |
| `Makefile` | `make test` (run smoke test), `make play` (start interactive game), `make build` |

## Prerequisites

- [Ollama](https://ollama.com) running locally with at least one model pulled (e.g. `qwen3.5:4b`, `llama3.2:3b`, etc.):
  ```bash
  ollama pull qwen3.5:4b
  ```

## Running the Interactive Game

```bash
make play
# or:
gxi -e '(import "game") (play)'
# or specify another model:
gxi -e '(import "game") (play model: "llama3.2:3b")'
```

Type actions at the `>` prompt. Type `quit` or `exit` to end the game.

## Running the Automated Smoke Test

```bash
make test    # or: gxi test.ss
```

Sample output from the smoke test:
```text
=== Testing Text Adventure Game with Ollama ===

[Turn 1: Opening scene request]
The air in the Valley of the Troll hangs thick and cold...
Current Status:
* Health: 100%
* Inventory: Travel pack, Waterskin, Sturdy staff
* Progress: Valley entrance

What will you do?
A) Follow the narrow path winding up toward the mountain lair.
B) Search the banks of the gurgling stream.
C) Call out into the mist to announce your presence.

[Turn 2: Player action -> 'look into stream']
You kneel at the muddy bank of the stream...
```
