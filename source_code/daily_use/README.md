# daily-use — Gemini REPL with Search & Cache

An interactive command-line tool built with Gerbil Scheme that provides a readline-enabled REPL for querying Google's Gemini API (`gemini-3-flash-preview`), with Google Search grounding and a persistent file-based cache for building LLM context.

## Prerequisites

- **Gerbil Scheme** (v0.18+) — `brew install gerbil-scheme` (macOS)
- **GNU readline** — ships with Gerbil/Gambit runtime
- **GOOGLE_API_KEY** environment variable set

## Quick Start

```bash
export GOOGLE_API_KEY=your-key-here

# Build the standalone binary
gxpkg build

# Or use the Makefile
make build

# Run
./.gerbil/bin/daily-use
```

Install system-wide:

```bash
make install
daily-use
```

## REPL Commands

| Input | Action |
|-------|--------|
| `<text>` | Ask Gemini a question |
| `!<text>` | Ask with Google Search grounding |
| `>` | Add last answer to persistent cache |
| `!` | Clear cache entries older than 1 week |
| `h` / `help` | Show help |
| `q` / `quit` | Exit |
| `Ctrl-D` | Exit |

## How It Works

- **Cache as context**: Cached entries relevant to your current query (matched by bag-of-words keyword overlap) are prepended to each prompt, giving Gemini targeted context from previous conversations.
- **Search grounding**: Prefix a query with `!` to enable Google Search, useful for current events or factual lookups.
- **Line editing**: Full GNU readline support via the Gambit runtime — arrow keys, history, Ctrl-R search, etc.
- **Persistent cache**: Answers are stored as s-expression pairs in `~/.daily-use-cache-gerbil.db`. Entries older than 7 days are purged with the `!` command.

## Dependencies

- `:std/net/request` — HTTP client for Gemini Interactions API
- `:std/text/json` — JSON encoding/decoding
- `:std/build-script` — `defbuild-script` for standalone binary compilation
- All dependencies ship with Gerbil Scheme's standard library

## Example run showing search, caching, then same query without search

```
$ ./.gerbil/bin/daily-use

  Gemini Daily-Use REPL  (type 'h' for help)

gemini> h

  Gemini Daily-Use REPL
  ─────────────────────────────────────────
  <text>         Ask Gemini a question
  !<text>        Ask with Google Search grounding
  >              Add last answer to cache
  !              Clear cache entries older than 1 week
  h / help       Show this help
  q / quit       Exit
  Ctrl-D         Exit
  ─────────────────────────────────────────
  Model: gemini-3-flash-preview
  Cache: ~/.daily-use-cache-gerbil.db (0 items)

gemini> !what sci-fi movies are playing today in Flagstaff AZ?
  [Searching...]

For today, Sunday, May 24, 2026, the following science fiction movie is
playing in Flagstaff, AZ:

*   **Project Hail Mary** (PG-13) is showing at the **Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or
your preferred ticketing platform to confirm specific showtimes, as they
can change throughout the day.

gemini> >
  [Cached. 1 items total]
gemini> what sci-fi movies are playing today in Flagstaff AZ?
  [Thinking...]

For today, Sunday, May 24, 2026, the science fiction movie
**Project Hail Mary** (PG-13) is playing at the
**Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or
your preferred ticketing platform to confirm specific showtimes, as they
can change throughout the day.

gemini> q
Goodbye.
  [Cache closed]
```

## Implementation Notes

The cache is stored as a flat file of s-expression pairs — one per line:

```scheme
(1748149200 . "previous answer text")
(1748149300 . "another cached answer")
```

Each entry is a `(timestamp . "text")` pair. The file is rewritten on every modification and read entirely into memory on startup. This is efficient for the intended use case (hundreds of entries).

## License

Apache 2.0
