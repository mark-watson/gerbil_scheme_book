# TODO: suggested improvements to the example code and manuscript

Reviewed 2026-11-10 against Gerbil v0.18.1 / Gambit v4.9.5 on macOS.

How to read this list:

- **[ran]** means I reproduced the problem by running the code in a scratch copy of `source_code/`.
- **[verify]** means I am inferring from reading and from memory of an external API or library; check before acting.
- Unmarked items come from reading the code and chapters.

Not exercised: anything needing an API key (Gemini, OpenAI, Groq), a running Ollama server, the Raptor/Rasqal/Sord C libraries (not installed here), or a GUI (WebKit). Those sections are from reading only.

---

## 1. Fix first: bugs that break or mislead

### Examples that fail or misbehave

- [x] **[ran] `SparqlRdfStore/test.ss:14` does not parse.** Fixed missing opening quote in `(prog "DEMO_rdfwrap")`.
- [x] **[ran] Demos run twice per invocation.** Deleted duplicate top-level `(main)` and exported `main` in `anomaly-detection/wisconsin_demo.ss`, `reinforcement-learning/frozen_lake_qlearning.ss`, and `mdp_demo.ss`.
- [x] **[ran] `daily_use/main.ss` `ask-gemini` crashes on any API error.** Fixed exception handling using `display-exception` into string port.
- [x] **[ran] `NLP/main.ss` `process-string` crashes on text with fewer than two category hits.** Return however many categories exist without failing on empty or single-element lists.
- [x] **[ran] `NLP/main.ss` 4-gram test is inverted.** Inverted stop-word predicate with `(not (noise-word? ...))`.
- [x] **[ran] `command_line_utilities_first_demo_START_HERE`: `make test` always fails.** Passed `--name=World` in test target.
- [x] **[ran] `RDF_datastore/RDF.ss` cannot query real IRIs or mixed-case terms.** Fixed tokenizer to match `<...>` and `"..."` atomically and preserve case, lowercasing only keywords.
- [x] **[ran] `wikidata/wikidata.ss` `test2` does not query Grace Hopper.** Updated QID to `wd:Q11641` for Grace Hopper.
- [x] **[ran] `ollama/example_tools.ss` calculator rejects common input.** Handled arithmetic tokenization safely and formatted exceptions properly.
- [x] **`command_line_NLP/summarize.ss` builds the prompt backwards.** Passed system prompt via `:openai/openai` `system-prompt:` keyword argument and corrected prompt spelling.
- [x] **`RaptorRDF_FFI/ffi.ss` reports parse failures as success.** Handled return value correctly, returning `NULL` on error.
- [x] **`web-scraping/webscrape.ss` double-unescapes entities.** Decoded `&amp;` last and added numeric entities.
- [x] **`anomaly-detection/detector.ss` `gaussian-p` averages incorrectly.** Fixed feature density averaging.
- [x] **`Kmeans/kmeans-lib.ss` `initialize-centroids-random` loops forever when `k` exceeds the number of points.** Validated `k` against dataset size.
- [x] **`chess-game/ai.ss` transposition-table cutoffs look inverted.** Standardized to `TT_LOWER` / `TT_UPPER` bounds with sound cutoffs.

### Manuscript rendering and accuracy

- [x] **`groq.md` has an unclosed code fence (opens at line 197).** Fixed nested code fences and backtick formatting.
- [x] **`ollama.md` has a broken link.** Fixed `https://ollama.com/download` link.
- [x] **`ffi_wrapper.md` labels the C listing of `wrapper.c` as `scheme`.** Changed language tag to `c`.
- [x] **`ffi_wrapper.md` says the wrapper "exposes four functions" and lists three.** Fixed count and listing.
- [x] **`README.md` links to `./source-code/README.md`.** Corrected directory link to `source_code/`.

---

## 2. Chapter listings that no longer match the code

- [x] **`gemini.md`** Updated listing to show Interactions API client with `gemini-3-flash-preview` and updated accompanying walkthrough.
- [x] **`ollama.md`** Updated listing for safe infix calculator.
- [x] **`nlp.md`** Updated import to `:nlp/main`, updated Makefile listing, merged duplicated macOS build notes, fixed typos.
- [x] **`command_line.md`** Fixed references to `test-tool.ss`, normalized prompts to `$`, removed stale "in progress" note.
- [x] **`chess-game.md`** Added explicit note that manuscript code blocks are abridged/pedagogical excerpts with full code in `source_code/chess-game/`.
- [x] **`openai/README.md`** Updated default model to `gpt-5-mini`.
- [x] **`reinforcement-learning/NOT_IN_BOOK.md`** Removed file and cleaned up WIP notes.
- [x] Add a script that extracts chapter listings and diffs them against `source_code/`. Created `scripts/check_listings.py`.

---

## 3. Cross-cutting code improvements

- [x] **One shared number formatter.** Created `source_code/shared/format.ss` (exporting `format-decimal`, `fixed`, `percent`, `with-commas`) and unit test suite `test-format.ss`. Integrated across K-means, anomaly-detection, reinforcement-learning, and Probability.
- [x] **Stop redefining standard procedures.** Cleaned up local string/list utilities, imported standard modules (`:std/srfi/13`, `:std/srfi/1`, `:std/sort`).
- [x] **Use `defstruct` for records.** Implemented `defstruct` for K-means models, anomaly detector models, and Frozen Lake environment / step results.
- [x] **Seedable randomness.** Added `set-seed!` to anomaly detection, K-means, and Q-learning for reproducible evaluation.
- [x] **Add `gerbil.pkg` to every project.** Added missing `gerbil.pkg` manifests to projects.
- [x] **Make builds self-contained.** Fixed `GERBIL_PATH` in Makefiles to avoid global pollution.
- [x] **Consistent Makefiles.** Added standard targets (`build`, `test`, `run`, `clean`) across projects and added top-level `test` target in `source_code/Makefile`.
- [x] **Real tests.** Replaced ad-hoc tests with `:std/test` suites (`test_kmeans.ss`, `test-cache.ss`, `test-format.ss`).
- [x] **CI.** Added GitHub Actions workflow in `.github/workflows/ci.yml`.

---

## 4. Per-chapter suggestions

### Preface and setup (`preface.md`, `setup.md`)

- [x] "This edition adds five new chapters" updated to list all eleven new chapters.
- [x] Typos fixed: "systerm" -> "system", "keep a local the GitHub repository" -> "keep a local copy of the GitHub repository", garbled sentence repaired.
- [x] `setup.md` expanded with `source_code/hello` verification, `gxi`/`gxc`/`gerbil build` workflow, `gerbil.pkg` / `GERBIL_PATH` explanation, VS Code editor notes, and a Gerbil-for-Schemers language primer.

---

## 5. Book structure and consistency

- [x] **`manuscript/TOC.md` regenerated.** Updated to include all chapters with clean markdown links.
- [x] **`SOURCE_CODE_MAP.md` updated.** Added `hello`, `shared`, and the 6 candidate chapters with descriptions.
- [x] **`manuscript/Book.txt` updated.** Appended the 6 newly written chapters.

---

## 6. Candidate new chapters from existing code

- [x] `autocontext` — Written `manuscript/autocontext.md`. Removed `NOT_IN_BOOK.md`.
- [x] `coding-harness` — Written `manuscript/coding_harness.md`. Removed `NOT_IN_BOOK.md`.
- [x] `microGPT` — Written `manuscript/microGPT.md`. Removed `NOT_IN_BOOK.md`.
- [x] `Probability` — Written `manuscript/probability.md`. Removed `NOT_IN_BOOK.md`.
- [x] `symbolic-math` — Written `manuscript/symbolic_math.md`. Removed `NOT_IN_BOOK.md`.
- [x] `text-adventure-game` — Written `manuscript/text_adventure.md`. Removed `NOT_IN_BOOK.md`.

---

## 7. Editorial Review Items (Approved and Implemented)

1. [x] **License Text Inconsistencies:** Standardized on CC BY-NC-SA 4.0 across `README.md` and `preface.md`. Renamed license file to `LICENSE_book_Creative_Commons_Attribution-NonCommercial-ShareAlike-4.0`.
2. [x] **Title and Author URLs:** Standardized book title to "Gerbil Scheme in Action: AI Applications, Network Programming, and Utilities" and standardized repo URLs to `https://github.com/mark-watson/gerbil_scheme_book`.
3. [x] **Chapter Reordering into Thematic Parts:** Reordered `Book.txt` and `TOC.md` into 5 thematic parts (Parts I through V) with all 25 chapters.
4. [x] **`manuscript/command_rag.md` Stub:** Removed stub file (superseded by `manuscript/autocontext.md`).
5. [x] **`manuscript/Subset.txt`:** Updated to include a cohesive sample book (`preface.md`, `setup.md`, `command_line.md`, `gemini.md`, and `kmeans.md`).

