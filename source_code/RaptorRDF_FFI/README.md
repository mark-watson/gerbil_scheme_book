# Gerbil Scheme FFI Example: Raptor RDF Library

**Book Chapter:** [Gerbil Scheme FFI Example Using the C Language Raptor RDF Library](https://leanpub.com/read/Gerbil-Scheme/gerbil-scheme-ffi-example-using-the-c-language-raptor-rdf-library) — *Gerbil Scheme in Action* (free to read online).

> **Note: currently configured for macOS only** (uses `raptor2` installed via Homebrew).

This example demonstrates Gerbil Scheme's **Foreign Function Interface (FFI)** by wrapping [Raptor2](https://librdf.org/raptor/), a well-established C library for parsing RDF files. It lets you parse any RDF file (Turtle, RDF/XML, N-Triples, etc.) and get back N-Triples as a Scheme string — all from within Gerbil, with no intermediate processes.

## What it does

`ffi.ss` embeds a small C function (`parse_file_to_ntriples`) inline using Gerbil's `begin-ffi` / `c-declare` mechanism, then exposes it to Scheme as:

```scheme
(raptor-parse-file->ntriples filename syntax-name)
; → N-Triples string, or #f on error
```

`test.ss` validates the FFI by writing a temporary Turtle file, parsing it with both explicit syntax (`"turtle"`) and auto-detection (`"guess"`), and asserting the expected N-Triples output.

## Prerequisites

- macOS with Xcode command-line tools
- Gerbil Scheme (`gxc`)
- Raptor2 via Homebrew:
  ```bash
  brew install raptor
  ```

## Build and run

```bash
make        # compiles ffi.ss and links against raptor2
./test      # runs the test suite
```

Expected output:

```
PASS turtle -> ntriples
PASS guess -> ntriples
All tests passed.
```

## Key FFI concepts demonstrated

| Concept | Where |
|---------|-------|
| `begin-ffi` block | Wraps all FFI declarations |
| `c-declare` | Embeds C source inline in the `.ss` file |
| `define-c-lambda` | Maps a C function to a Scheme identifier |
| `char-string` type | Passes/returns C `char*` as Scheme strings |

## Why this matters

Most real-world Scheme programs need to call existing C libraries. Gerbil's FFI lets you do this without writing a separate shared library or using subprocess hacks. The Raptor2 example is a realistic use case: parse industry-standard RDF data directly in Scheme, enabling integration with Linked Data and Semantic Web tooling.
