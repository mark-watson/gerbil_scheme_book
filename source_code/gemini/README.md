# Google Gemini API Examples

**Book Chapter:** [Google Gemini API](https://leanpub.com/read/Gerbil-Scheme/google-gemini-api) — *Gerbil Scheme in Action* (free to read online).

A minimal, reusable Gerbil Scheme library for calling the [Google Gemini](https://ai.google.dev/) generative AI API. The single `gemini` procedure sends a prompt and returns the model's text response as a Scheme string — no third-party dependencies beyond Gerbil's standard library.

## Prerequisites

- Gerbil Scheme (`gxi`/`gxc`)
- A Google AI API key — set the environment variable:
  ```bash
  export GOOGLE_API_KEY="your-key-here"
  ```

## Files

| File | Description |
|------|-------------|
| `gemini.ss` | Library module — exports `gemini` procedure |
| `gerbil.pkg` | Package declaration (`gemini`) |
| `Makefile` | `run` target — loads the library in an interactive REPL |

## How to run

### Interactive REPL

```bash
make run
# or equivalently:
gxi -L gemini.ss -
```

Then call the function at the prompt:

```scheme
> (import "gemini")
> (gemini "Why is the sky blue? Be concise.")
"The sky is blue because of Rayleigh scattering ..."
```

### Import from another module

```scheme
(import "gemini")   ; or :gemini/gemini if installed as a package

(displayln (gemini "Summarize the Scheme programming language in one sentence."))
```

## API

```scheme
(gemini prompt
        model: "gemini-2.5-flash"          ; optional — any Gemini model name
        system-prompt: "You are a helpful assistant.")  ; optional
```

Returns the response text as a string, or raises an error on API failure.

## Notes

- Uses `https://generativelanguage.googleapis.com/v1beta/models/<model>:generateContent`
- Default model is `gemini-2.5-flash` — fast and inexpensive for most tasks
- The API key is read at call time from `GOOGLE_API_KEY`; no key is baked into the source
