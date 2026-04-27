# Ollama Local LLM Inference

**Book Chapter:** [Ollama](https://leanpub.com/read/Gerbil-Scheme/ollama) — *Gerbil Scheme in Action* (free to read online).

A minimal Gerbil Scheme library for running prompts against locally-hosted language models via [Ollama](https://ollama.com). No API key or internet connection required — the model runs entirely on your machine.

## Prerequisites

- Gerbil Scheme (`gxi`/`gxc`)
- [Ollama](https://ollama.com) installed and running locally
- At least one model pulled, e.g.:
  ```bash
  ollama pull gemma3        # ~2 GB — good general-purpose default
  ollama pull qwen3:0.6b   # ~500 MB — very fast, lower quality
  ```

## Files

| File | Description |
|------|-------------|
| `ollama.ss` | Library module — exports `ollama` procedure |
| `gerbil.pkg` | Package declaration |

## How to run

### Interactive REPL

```bash
gxi
> (import "ollama")
> (ollama "Why is the sky blue? Be very concise.")
"The sky appears blue due to Rayleigh scattering ..."
```

### Use a different model

```scheme
(ollama "Write a haiku about Scheme." model: "qwen3:0.6b")
```

## API

```scheme
(ollama prompt
        model: "gemma3:latest")   ; optional — any model you have pulled
```

Returns the response text as a string, or raises an error if Ollama is not running or the model is not available.

## Notes

- Connects to `http://localhost:11434/api/generate` (Ollama's default address)
- Uses non-streaming mode (`"stream": false`) so the full response is returned as one string
- Ollama must be running (`ollama serve`) before calling this function
- To see available models: `ollama list`
- This is ideal for private/offline use cases where you cannot or do not want to send data to a cloud API
