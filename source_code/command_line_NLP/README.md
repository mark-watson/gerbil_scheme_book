# Command-Line NLP Applications

**Book Chapter:** [Command Line Applications For NLP](https://leanpub.com/read/Gerbil-Scheme/command-line-applications-for-nlp) — *Gerbil Scheme in Action* (free to read online).

This directory shows how to build standalone command-line executables on top of the NLP library developed in the `NLP/` chapter. It demonstrates two patterns:

1. **Importing an existing Gerbil package** — using `:nlp/main` from the `NLP/` library directly in the REPL or in another compiled program.
2. **Reading command-line arguments as a string** — a simple utility pattern useful for piping text into NLP tools.

## Prerequisites

- Gerbil Scheme (`gxi`/`gxc`)
- The `NLP/` package must be compiled and installed first:
  ```bash
  cd ../NLP && make
  ```

## Files

| File | Description |
|------|-------------|
| `categories.ss` | CLI tool: reads a text file and prints its top NLP categories |
| `summarize.ss` | CLI tool: reads a text file and prints extracted key phrases |
| `test_command_string.ss` | Minimal demo: echoes all CLI arguments as a single string |
| `test.ss` / `test2.ss` | REPL test scripts demonstrating the NLP library API |

## How to run

### Interactive REPL — call the NLP library directly

```bash
gxi
> (import :nlp/main)
> (nlp/main#process-string
    "President Biden went to Congress to discuss the economy and the budget deficit.")
```

Returns a list: `(words pos-tags key-phrases top-categories summary-words names places)`

### Compile and run the argument-echo utility

```bash
gxc -O -exe -o test_command_string test_command_string.ss
./test_command_string the dog ran fast. 1 2 3
# → the dog ran fast. 1 2 3
```

### Compile and run the categories tool

```bash
gxc -O -exe -o categories categories.ss
./categories path/to/document.txt
```

The compiler prints intermediate `.scm` and `.c` filenames as it compiles the transitive closure of imports — this is normal Gerbil output, not errors.

## How Gerbil package imports work

When you write `(import :nlp/main)`, Gerbil looks for the compiled package in `~/.gerbil/lib/nlp/`. The `NLP/` project's `make` target installs it there. This is the same mechanism used by all multi-file Gerbil projects in this book.