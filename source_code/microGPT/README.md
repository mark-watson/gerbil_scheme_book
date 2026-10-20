# microGPT: A Tiny GPT from Scratch in Gerbil Scheme

A port of Andrej Karpathy's **microGPT**, written with no dependencies beyond the Gerbil standard library. It is based on the Common Lisp version in *Loving Common Lisp*. The whole thing is one file of about 330 lines and contains everything a GPT needs:

1. **Autograd engine**: a scalar `value` struct that records the computation graph. `backward` walks the graph in topological order and applies the chain rule, in the same way as Karpathy's *micrograd*.
2. **Transformer**: token and position embeddings, RMSNorm, 4-head causal self-attention with a KV cache, a ReLU MLP, and residual connections.
3. **Training**: character-level next-token prediction on ~32,000 first names, using cross-entropy loss and the Adam optimiser with bias correction and linear learning-rate decay.
4. **Inference**: temperature-scaled (0.5) sampling that "hallucinates" new names.

## Files

| File | Description |
|------|-------------|
| `microgpt.ss` | Autograd engine, model, training loop, sampler, and `main` |
| `names.txt` | Training corpus, one name per line |
| `Makefile` | `make run` (compile and train for 1000 steps), `make test` (short interpreted run) |

## Running

```bash
make run              # compile an optimized executable and train 1000 steps (~17 s on an M-series Mac)
./microgpt 300        # custom number of training steps
./microgpt 500 my.txt # custom corpus
make test             # interpreted with gxi, 20 steps (~0.25 s per step)
```

Example output after 1000 steps:

```text
num docs: 32033
vocab size: 27
num params: 4192
step 1/1000 | loss 3.431
...
step 1000/1000 | loss 2.425

--- inference (new, hallucinated names) ---
sample 1: mamare
sample 2: margoe
sample 7: marian
sample 8: zaida
sample 18: layla
```

The loss starts near ln(27) ≈ 3.3, which is what random guessing over the vocabulary would score, and falls to about 2.4.

## Model Configuration

| Parameter | Value |
|-----------|-------|
| `n-layer` | 1 |
| `n-embd` | 16 |
| `n-head` / `head-dim` | 4 / 4 |
| `block-size` | 16 |
| Parameters | 4,192 |

## Porting Notes (Common Lisp to Gerbil)

- CL `defstruct value` became Gerbil `(defstruct value (data grad children local-grads))`, and the generated `value-grad-set!` setters replace `setf`/`incf`.
- The `eq` hash table used for the topological sort is created with `make-hash-table-eq`.
- Adjustable CL arrays with fill pointers (the KV cache) became a vector of lists, one per layer.
- `gxi microgpt.ss` runs `main` directly. Compiling with `gxc -O -exe` is roughly 10x faster, because every scalar operation allocates a graph node.
- The Makefile adds `-L/opt/homebrew/opt/openssl@3/lib` to the link options because some Homebrew Gerbil builds still point at an OpenSSL version that has since been removed.
