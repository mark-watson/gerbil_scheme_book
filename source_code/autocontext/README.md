# AutoContext: Hybrid Context Retrieval for LLM Prompts

AutoContext demonstrates how to build effective, focused prompts from large text datasets that would not fit in a single LLM context window. It combines sparse keyword search (**BM25 Okapi**) and vector-space similarity search (**TF-IDF with cosine distance**) to locate relevant text chunks, merges and deduplicates them by chunk index, and constructs a structured context prompt.

Ported to pure Gerbil Scheme from the Common Lisp `autocontext` example in *Loving Common Lisp*.

## Pipeline

1. **Document Loading & Chunking**: reads `data/*.txt` files, splits them into sentences via heuristic boundary detection, and groups them into chunks (default 3 sentences).
2. **Hybrid Retrieval**:
   - **BM25 (`bm25.ss`)**: Okapi BM25 scoring with term-frequency and inverse document frequency.
   - **TF-IDF Vector Space (`tfidf.ss`)**: sparse vector representations weighted by log term frequency and smoothed IDF, compared using cosine similarity. (In Common Lisp this used a Python sentence-transformer helper; this Gerbil version is 100% self-contained Scheme).
3. **Prompt Construction**: takes top-N hits from both retrievers, deduplicates by original chunk index so repeated hits reinforce rather than duplicate text, and builds a standard RAG/context prompt.
4. **LLM Inference**: optionally pipes the resulting prompt directly to a local Ollama model (e.g. `qwen3.5:4b`).

## Files

| File | Description |
|------|-------------|
| `bm25.ss` | BM25 Okapi scoring index |
| `tfidf.ss` | TF-IDF sparse vector generator and cosine similarity search |
| `autocontext.ss` | Text chunker, tokenizer, index coordinator, prompt builder, and Ollama bridge |
| `test.ss` | Test driver that builds context from `data/` and queries Ollama |
| `data/` | Sample text files (`economics.txt`, `chemistry.txt`, `sports.txt`, `health.txt`) |

## Running

```bash
make test     # or: gxi test.ss
make build    # compile with gxc
```

### Example Output

```text
Initializing AutoContext from directory: data
Loaded 22 text chunks.
Building BM25 and TF-IDF retrievers...
--- Retrieving context for query: 'who says that economics is bullshit?' ---
BM25 found 2 keyword-based results.
TF-IDF search found 2 similarity-based results.
Combined and deduplicated, we have 2 context chunks.

=== Generated Prompt for LLM ===
Based on the following context, please answer the question:
who says that economics is bullshit?

--- CONTEXT ---
... An interesting Economist is Pauli Blendergast who teaches at the University of Krampton Ohio and is famouse for saying economics is bullshit.
---
--- END CONTEXT ---

=== Querying Ollama (qwen3.5:4b) ===
Based on the text provided, Pauli Blendergast, an economist who teaches at the University of Krampton Ohio, is the one said to be famous for saying that economics is bullshit.
```

## API

```scheme
(import "autocontext")

;; Build indices from a directory of .txt files
(def ac (make-auto-context "data"))

;; Generate prompt
(def prompt (get-prompt ac "who says that economics is bullshit?" num-results: 2))

;; Or ask an Ollama model directly
(def answer (ask-with-context ac "who says that economics is bullshit?" model: "qwen3.5:4b" num-results: 2))
```
