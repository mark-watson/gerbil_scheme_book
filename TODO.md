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

- [ ] **[ran] `SparqlRdfStore/test.ss:14` does not parse.** `(prog DEMO_rdfwrap")` is missing its opening quote, so the reader hits EOF and `make` cannot build `TEST_client`. The listing in `ffi_wrapper.md` is correct; the source file is not.
- [ ] **[ran] Demos run twice per invocation.** `anomaly-detection/wisconsin_demo.ss`, `reinforcement-learning/frozen_lake_qlearning.ss` and `mdp_demo.ss` define `main` and also call `(main)` at top level. `gxi file.ss` auto-applies `main`, so each trains and prints twice. Delete the trailing `(main)` and `(export main)`.
- [ ] **[ran] `daily_use/main.ss` `ask-gemini` crashes on any API error.** The handler does `(string-append "[Error calling Gemini API: " e "]")` with `e` an exception object, which raises `STRING expected` inside the handler. Format the exception with `display-exception` into a string port.
- [ ] **[ran] `NLP/main.ss` `process-string` crashes on text with fewer than two category hits.** `(list (car category-seq) (cadr category-seq))` raises `(car '())` for input like `"xyzzy plugh"`. This also takes down the `categories` command-line tool. Return however many categories exist.
- [ ] **[ran] `NLP/main.ss` 4-gram test is inverted.** `(if (noise-word? (vector-ref words (+ 3 n))) ...)` records a 4-gram only when the fourth word *is* a stop word; it needs `not`. Observed key phrase: `"red dog ran. the"`.
- [ ] **[ran] `command_line_utilities_first_demo_START_HERE`: `make test` always fails.** It runs `gxi test-tool.ss` with no `--name`, so the tool prints usage and exits 2. Pass `--name=World`.
- [ ] **[ran] `RDF_datastore/RDF.ss` cannot query real IRIs or mixed-case terms.** The tokenizer splits on `.` and the parser lowercases every token, so `<http://example.org/Alice>` never matches and `"Alice Smith"` is split in two. The demo only works because its data is lowercase `<ex:...>` terms. Tokenize `<...>` and `"..."` atomically and lowercase only keywords.
- [ ] **[ran] `wikidata/wikidata.ss` `test2` does not query Grace Hopper.** `wd:Q7249` returns birth date 1862-02-14, birthplace Venice. The code comment and `wikidata.md` both say Grace Hopper (born 1906, New York City). **[verify]** I believe her QID is Q11641.
- [ ] **[ran] `ollama/example_tools.ss` calculator rejects common input.** `"42*17"` (no spaces) and `"3 -2"` both fail, `"7 / 2"` returns the rational `7/2`, and the error string is empty because `display-exception` does not write into the `with-output-to-string` port. Tokenize characters instead of using `read`, return inexact results, and build the message from the exception.
- [ ] **`command_line_NLP/summarize.ss` builds the prompt backwards.** `(string-append input-str system-prompt)` puts "HERE IS THE TEXT TO SUMMARIZE:" *after* the text. Pass the instructions through `openai`'s `system-prompt:` keyword. The prompt also has typos ("You ar a master at libguistics").
- [ ] **`RaptorRDF_FFI/ffi.ss` reports parse failures as success.** The return value of `raptor_parser_parse_file` is ignored, so a missing file yields `""`. `parse-rdf-file` only raises on `#f`, so its documented "raises on error" behaviour never triggers, and `test.ss` case 5 asserts the empty string. Return `NULL` on failure and update the test and `ffi_raptor.md`.
- [ ] **`web-scraping/webscrape.ss` double-unescapes entities.** `&amp;` is decoded before `&lt;`, so `&amp;lt;` becomes `<`. Decode `&amp;` last and add numeric entities (`&#8217;`, `&#x27;`).
- [ ] **`anomaly-detection/detector.ss` `gaussian-p` averages incorrectly.** It sums `nf - 1` feature densities and divides by `nf`.
- [ ] **`Kmeans/kmeans-lib.ss` `initialize-centroids-random` loops forever when `k` exceeds the number of points.** Validate `k` in `kmeans-fit`.
- [ ] **[verify] `chess-game/ai.ss` transposition-table cutoffs look inverted.** Fail-low results are stored as `TT_BETA` and fail-high as `TT_ALPHA`, but the probe cuts off on `TT_ALPHA` when `score <= alpha` and on `TT_BETA` when `score >= beta`. For a lower bound (fail-high) the sound cutoff is `score >= beta`, and vice versa. Rename the flags `TT_LOWER`/`TT_UPPER` and recheck the probe.

### Manuscript rendering and accuracy

- [ ] **`groq.md` has an unclosed code fence (opens at line 197).** The sample Haskell output contains a line with three backticks, which closes the block early; the final fence then swallows "Optional Practice Problems". Use a four-backtick or tilde fence. The same output shows a two-backtick opener, an artifact of working around this.
- [ ] **`ollama.md` has a broken link:** `[https://ollama.com/download}(https://ollama.com/download)`.
- [ ] **`ffi_wrapper.md` labels the C listing of `wrapper.c` as `scheme`** (line 243).
- [ ] **`ffi_wrapper.md` says the wrapper "exposes four functions" and lists three.**
- [ ] **`README.md` links to `./source-code/README.md`.** The directory is `source_code` and has no top-level README.

---

## 2. Chapter listings that no longer match the code

- [ ] **`gemini.md`** shows the old `generateContent` client (`gemini-2.5-flash`, `candidates`/`parts`, error text naming `GEMINI_API_KEY`). `gemini.ss` now calls the Interactions API (`/v1beta/interactions`, `input`, `steps`, `Api-Revision`, `gemini-3-flash-preview`). Replace the listing and the two paragraphs that walk through it.
- [ ] **`ollama.md`** shows `calculate-handler` using `eval` and describes that design. The code now has a restricted infix evaluator. Update the listing; keep a short note on why `eval` on model output is unsafe. The tool description still says "Math expression like (+ 2 2)".
- [ ] **`nlp.md`** shows `(import :kbtm/main)` and a Makefile using `HOME=$(CURDIR) gxi ...`, with three paragraphs explaining that trick. The code imports `:nlp/main` and the Makefile no longer sets `HOME` (a stale comment remains).
- [ ] **`command_line.md`** refers to `test-mytool.ss` (file is `test-tool.ss`) and to `manifest.ss`, which is gitignored.
- [ ] **`chess-game.md`** has many listings that differ from source in comments and line breaks. Decide whether listings are verbatim or abridged and say so.
- [ ] **`openai/README.md`** gives the default model as `gpt-4o-mini`; the code default is `gpt-5-mini`.
- [ ] **`reinforcement-learning/NOT_IN_BOOK.md`** exists although `ReinforcementLearning.md` is in `Book.txt`. Remove it, and the "# work in progress" lines in that Makefile and in `web-scraping/Makefile`.
- [ ] Add a script that extracts chapter listings and diffs them against `source_code/`, and run it before each publish. Better: include listings from the source files so they cannot drift.

---

## 3. Cross-cutting code improvements

- [ ] **One shared number formatter.** `format-decimal` is copied into `detector.ss`, `kmeans-lib.ss`, `frozen_lake_qlearning.ss`, `mdp_demo.ss` and `Probability/fmt.ss`, and the copies behave differently. **[ran]** the K-means copy prints `-.50` for -0.5 and `9.99` for 1e21; the RL copy prints `.74`. Move one tested version into a shared module.
- [ ] **Stop redefining standard procedures.** Local copies of `string-trim`, `string-split`, `string-index`, `string-downcase`, `string-prefix?`, `string-suffix?`, `char-whitespace?`, `string-ci=?`, `sort`, `take` and `any` appear across `NLP/utils.ss`, `RDF.ss`, `daily_use`, `detector.ss`, `webscrape.ss` and `chess-game/cli.ss`. Import from `:std/srfi/13`, `:std/srfi/1`, `:std/sort`, `:std/misc/string`. `daily_use/cache.ss` uses `(export #t)` and so exports its own `take` and `any`.
- [ ] **Use `defstruct` for records.** Models and environments are hash tables or bare vectors (`detector.ss`, `kmeans-lib.ss`, FrozenLake env, WebKit app record, Ollama tool records). Structs give accessors and clearer errors, and the book would then demonstrate a core Gerbil feature.
- [ ] **Strings in body position are not docstrings.** Many functions start with a `"..."` string as in Common Lisp. In Gerbil it is a discarded expression. Convert to `;;` comments, or explain the convention once.
- [ ] **One HTTP/LLM helper.** `openai.ss`, `groq_inference.ss`, `gemini.ss`, `ollama.ss`, `use_tools.ss` and `daily_use/main.ss` repeat the same build-headers / post / check-status / dig-out-text code with different error handling. `gemini.ss` returns void on a non-200 response while the others raise. `daily_use` reimplements the Gemini client instead of importing it. Extract a small `llm-common` module (JSON post, uniform error, optional timeout and retry) and build a chapter section around it.
- [ ] **Gemini API key is sent twice.** `gemini.ss` and `daily_use/main.ss` send the key in the `x-goog-api-key` header and again as `?key=` in the URL, where it can leak into logs. Drop the query parameter.
- [ ] **Guard JSON digging.** `openai.ss` calls `hash-ref` on a possibly-`#f` first choice; `gemini.ss` and `daily_use` call `hash-ref` on a possibly-`#f` `model-step` and read only the first content part. `groq_inference.ss` already shows the safer pattern.
- [ ] **Seedable randomness.** Anomaly detection, K-means and Q-learning give different numbers each run. Add an optional seed so the chapter output can be reproduced.
- [ ] **Remove unused imports and dead code.** Examples: `:std/pregexp` in `detector.ss` and `frozen_lake_qlearning.ss`; `:std/text/json` and `:std/net/uri` in `webscrape.ss`; `:std/cli/getopt` in `command_line_utilities/main.ss`; `:std/iter` in `summarize.ss`; `pprint-hashtable` and the unused `system-prompt` parameter in `gemini.ss`; `query-dbpedia` in `wikidata.ss`.
- [ ] **Add `gerbil.pkg` to every project.** Missing in `RaptorRDF_FFI`, `SparqlRdfStore`, `webkit-gerbil`, `command_line_NLP` and the first command-line demo. Without it `gxc ffi.ss` installs a global module literally named `ffi` (and `test`) in `~/.gerbil/lib`, which is visible in the directory listing printed in `command_line.md`.
- [ ] **Make builds self-contained.** `command_line_NLP` needs `:nlp/*` and `:openai/openai` already compiled into `~/.gerbil/lib` from other directories, and `groq` needs a prior `gxc`. Add a `deps` target, or set `GERBIL_PATH` to a project-local directory so `make clean` really cleans.
- [ ] **Consistent Makefiles.** Give every project `build`, `test`, `run` and `clean`. The top-level `source_code/Makefile` only has `clean`; add `test` to run every offline test. `install` targets use `mv` into `/usr/local/bin`; use `install -m 755`. `groq_llm_inference/Makefile` lacks a trailing newline and its `clean` leaves compiled output behind.
- [ ] **Real tests.** Use `:std/test` in place of hand-rolled `assert-equal`/`report`. Remove `Kmeans/test2.ss`, which `include`s the library to dodge an import problem. Delete or fix leftovers: `command_line_NLP/test2.ss` (wrong comment, unused helper), `test.ss` (computes but prints nothing), `test_command_string.ss`.
- [ ] **CI.** A GitHub Action that installs Gerbil and runs the offline tests (K-means, anomaly detection, RDF store, chess perft, RL, NLP smoke test) would have caught most of section 1.

---

## 4. Per-chapter suggestions

### Preface and setup (`preface.md`, `setup.md`)

- [ ] "This edition adds five new chapters" then lists six.
- [ ] Typos: "systerm", "keep a local the GitHub repository", and the garbled sentence "from the complex and we will in later chapters".
- [ ] License text disagrees in three places: the preface says Attribution–ShareAlike Non-Commercial, the README says "no modifications" (which would be ND), and the license file is BY-NC-SA. The README also says "All rights reserved" beside a CC grant, and the license filename has a typo (`COmmons`).
- [ ] The LGPL summary says commercial use is "without restrictions"; soften it, since LGPL has relinking and notice obligations for static linking, which is how `gxc -exe` links.
- [ ] The macOS deployment-target section is dated "September 2025 ... Tahoe 26.0 Beta". Recheck on current macOS and Gerbil and either update or remove; `nlp.md` repeats it twice.
- [ ] `setup.md` is 99 words and Emacs-only. Add: verifying the install with `source_code/hello` (which has a README pointing at this chapter but is never mentioned in it), the `gxi` / `gxc` / `gerbil build` workflow, how `gerbil.pkg` maps to import paths, where compiled modules go, and a note on VS Code or other editors. Fix "Here cover".
- [ ] Add a short Gerbil-for-Schemers primer (`def`, keyword and optional arguments, `defstruct`, `match`, `try`/`with-catch`, hash tables, `for`). Every later chapter assumes these.

### LLM chapters (`gemini.md`, `ollama.md`, `openai.md`, `groq.md`)

- [ ] Each chapter opens with a long marketing-style paragraph that is already dated (Gemini Ultra/Nano, GPT-3/GPT-4, Llama 3, Phi-3). Cut to two or three sentences and avoid naming model generations.
- [ ] Say how to set `GOOGLE_API_KEY`, `OPENAI_API_KEY` and `GROQ_API_KEY` before the first REPL example.
- [ ] `gemini.md`: "Change directory to **source-code/gemini** an run". Practice problem 2 (send the system prompt) is a gap in the example itself; implement it.
- [ ] `openai.md`: the chapter covers Chat Completions only; mention the Responses API or state the choice. The "one-shot" example contains no worked example in the prompt, so it is really a format specification.
- [ ] `groq.md`: typos "LMS", "paramters", "triliion", "comiled". **[verify]** Kimi K2 was released under a Modified MIT license, so "Open-Weight (proprietary)" is probably wrong. **[verify]** the practice problems name `llama3-70b-8192`, `mixtral-8x7b-32768` and `llama3-8b`, which I believe Groq has retired; check the current model list, including `moonshotai/kimi-k2-instruct`.
- [ ] `ollama.md`: trim the very long `ollama serve` log line. `/api/chat` is described as "OpenAI-compatible"; it is Ollama's native API (the compatible one is `/v1/chat/completions`). Practice problem 2 asks for a tool that already exists.
- [ ] `ollama` code: read the host from `OLLAMA_HOST`; include `tool_name` in tool-result messages; `tool-registry-schemas` returns void for unknown names (filter them); return a message when `max-rounds` is exhausted instead of raising; catch connection-refused and say "is Ollama running?".
- [ ] Consider one streaming example across the four chapters; it is the first practice problem in `gemini.md` and the most common real need.

### Wikidata (`wikidata.md`)

- [ ] The 211-line listing is half duplication. `query-wikidata` and `query-wikidata/alist` differ in one call, and `test1-ua`/`test2-ua` copy `test1`/`test2`. Use one query function with `user-agent:` (defaulting to `WDQS_UA`) and `as:` keywords.
- [ ] The default User-Agent is a placeholder `example.org` contact, which does not meet the WDQS policy the chapter cites.
- [ ] `test1` finds entities by exact `rdfs:label`, which is slow and can time out; show `wikibase:mwapi` EntitySearch or explain the cost.
- [ ] Handle HTTP 429 with `Retry-After`. Keep `datatype` and `xml:lang` from the bindings instead of discarding them.
- [ ] The Makefile listing begins with a stray `$ cat Makefile` line inside the fence.

### NLP (`nlp.md`) and NLP command-line tools (`command_nlp.md`)

- [ ] "macOS Specific Build Notes" and "Test Run" say the same thing twice; merge. Typos "probelems", "interpretter".
- [ ] The chapter prints `nlp.ss` (argument parsing and JSON output) and says "We will not discuss" the eight files that do the NLP. Walk through at least `fasttag.ss` and the category scoring; that is the interesting material.
- [ ] `nlp.ss` hand-writes a JSON serializer although the preface presents `:std/text/json` as the standard tool. Use it, or explain why not.
- [ ] `nlp.ss` `main` ignores its `argv`, prints help twice with `-h`, and returns 0 on bad arguments. Use `:std/cli/getopt` as taught in `command_line.md`. Help text still says "KBtextmaster".
- [ ] Stubs presented as features: `summarize.ss` returns `#f`; `place-names.ss` is "TBD", uses `for` without importing it and loads name files; `key-phrases.ss` has a malformed `import` form and inverted print logic. None is compiled by the Makefile. Finish or delete them. `summary-words`, `place-list` and the `1-gram` table in `main.ss` are computed or declared and never used.
- [ ] Category scoring is duplicated in `main.ss` and `category.ss` (`get-word-list-category` is never called), and category names leak a `.txt` suffix that `categories.ss` strips afterwards.
- [ ] Tokens keep punctuation (`"ran."`), which hurts n-grams and lookups.
- [ ] The repo carries about 6 MB of lexicon data twice (`data/*.dat`, `lexicon.txt` and `generated-code/*.ss`) plus training files with spaces in their names. Keep the generated files and the generator script; document the rest or drop it.
- [ ] `command_nlp.md`: typos "interpretivey", "GPT-5 odel". Console listings include `/tmp/gxc...` compiler noise; trim.

### FFI: Raptor (`ffi_raptor.md`)

- [ ] **Memory leak on every call.** `parse_file_to_ntriples` returns a `strdup`'d buffer through a `char-string` result. Gambit copies the C string and does not free it. The chapter's memory-management paragraph says the opposite. Free the buffer in the `c-lambda` body after `___return` (**[verify]** the `___AT_END` idiom in the Gambit manual) and correct the text.
- [ ] **[verify]** `RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES` does not appear to be a real Raptor identifier, so the `#ifndef` fallback always applies and passes `0x4`, which I believe is the "allow UTF-8" bit flag, not N-Triples escaping. Consider `raptor_term_ntriples_write` or `raptor_statement_ntriples_write`.
- [ ] Use `UTF-8-string` for the filename and result; `webkit.md` already gives this advice.
- [ ] Round-tripping through N-Triples text and re-parsing by splitting on spaces is fragile. A stronger example would hand each term to Scheme from the statement handler through a `c-define` callback, which also teaches the callback half of the FFI.
- [ ] Add a `make test` target that runs `./test`.

### FFI: Rasqal and Sord wrapper (`ffi_wrapper.md`)

- [ ] **Sord is loaded and never queried.** `rdf_init` parses the file into a Sord model, but `rdf_query` has Rasqal re-read the file from disk on every query. The chapter title and "Library Selection" section present Sord as the store. Either drop Serd/Sord, which halves the dependencies, or state plainly that Sord only validates the load.
- [ ] Leaks: `file_uri` is never freed on the success path; calling `rdf_init` twice, or failing a load, leaks the world, model and env; the `char-string` result is never freed (same issue as Raptor). `rdf_query_copy` only adds a second copy, and the paragraph justifying it is wrong.
- [ ] A query with no rows returns the TSV header line, because the header is stripped only when a row follows.
- [ ] Merge `Makefile` and `Makefile.linux` with a `uname` switch; the macOS one hardcodes `/opt/homebrew` (Apple Silicon only). Add `libRDFWrap.so` to `.gitignore`.
- [ ] Two full Makefiles take about 190 lines of the chapter; show the key rules and link to the files.
- [ ] Fix the path `~/Users/markw/~GITHUB/...` and typos "direcory", "is n the".
- [ ] Global state makes the wrapper single-store and not thread-safe; say so, or pass a handle.

### WebKit (`webkit.md`)

- [ ] **[verify]** `c_bridge_trampoline` `strdup`s the string returned by the `c-define` callback but never releases the original. Gambit documents `___release_string` for strings returned from `c-define`.
- [ ] `dispatch-bridge-command` formats exceptions with `(format "~a" e)`, which prints an object tag, not the message; and `escape-json-string` misses control characters. Build the error reply with `json-object->string`.
- [ ] The "Markdown viewer" shows raw text in a `<pre>`; it does not render Markdown. Rename it or render it.
- [ ] The viewer's `read-file` handler reads any path JavaScript sends. Restrict it to the listed directory and `.md` files, and say why. It also builds `onclick` attributes by string concatenation and waits for the bridge with `setTimeout(..., 300)`; use `addEventListener` and a ready event.
- [ ] "`char-string` only handles ASCII": it is Latin-1.
- [ ] Practice problems 1 and 3 ask for the counter app and for `set-title`/`set-size`, which already exist.
- [ ] This chapter uses tilde fences and `{lang=...}` attributes; the rest of the book uses backtick fences. Pick one style.

### In-memory RDF store (`RDF_simple.md`)

- [ ] Beyond the tokenizer bug in section 1: the demo runs at load time, so importing `:rdf-datastore/RDF` as a library, as the chapter recommends, prints the demo. Move it into `main` or a separate file.
- [ ] `remove-triple` is documented as removing the first match and removes all matches. `add-triple` allows duplicates, although an RDF graph is a set.
- [ ] The complexity claim (time proportional to triples times patterns) is wrong: a nested-loop join is, in the worst case, triples to the power of patterns.
- [ ] The text presents lowercasing all tokens as "an acceptable simplification"; it makes the store unusable with real data. Fix it and delete the paragraph.
- [ ] Consider moving this chapter before the two FFI chapters, since it introduces RDF and SPARQL from first principles and they assume both.

### Command-line utilities (`command_line.md`)

- [ ] The introduction praises `getopt` and multi-call support, then the "flexible" example hand-parses `argv` (and practice problem 2 asks the reader to fix it). Use `getopt` subcommands in the example.
- [ ] Explanations to correct: `gxi -d` is not the package manager; `.o1`, `.o2` suffixes are Gambit object-file versions that increase on each recompile, not optimization passes; `~0` files are compiled module phases, not backups; `.ssi` is the module interface file.
- [ ] Remove "the next two chapters (currently being written)". Typos "ccommand-line", "/.gerbil/lib", "LLMS".
- [ ] Console transcripts show the hostname and home directory (`Marks-Mac-mini`, `/Users/markw/...`); normalize prompts to `$`.
- [ ] `test-tool.ss`: the usage string says `mytool`, and `(displayln (format "Hello, ~a\n" name))` prints a blank line.
- [ ] Rename `gerbil.pkg`'s `(package: markw)` to a project name. `cmd-ls` wraps `directory-files` in a `try` with a shell fallback that can never be needed.
- [ ] The `hello` architecture diagram appears here, though `hello` belongs to the setup chapter.

### Anomaly detection (`anomaly-detection.md`)

- [ ] The split is described as 60/20/20. The code gives roughly 60/28/12, and benign-filtering shrinks the training set further (**[ran]** 271/184/73 of 648).
- [ ] `preprocess-wisconsin` min-max scales each *row*, which removes overall magnitude, the main signal separating malignant from benign. Scale per feature using training-set statistics, or justify the choice.
- [ ] Epsilon is chosen by raw error count with ties going to the largest value, while the chapter motivates precision, recall and F1. Select on F1.
- [ ] `train-helper` recomputes the variance vector for each of the 200 candidate epsilons; compute it once in `build-detector`.
- [ ] `num-features` is 10 but means nine features plus the label; every loop carries a `(- nf 1)`. Separate features from labels.
- [ ] The score is the mean of per-feature densities, not their product. Since practice problem 1 asks for the product, show the comparison in the chapter.

### K-means (`kmeans.md`)

- [ ] `kmeans-single-run` computes inertia with centroids updated after the last assignment, so labels and inertia can be one step out of sync when `max-iters` is hit. Reassign once at the end.
- [ ] Empty clusters keep their previous centroid; mention reseeding to the farthest point.
- [ ] Add an elbow or silhouette example for choosing `k`.

### Chess (`chess-game.md`)

- [ ] `perft.ss` checks only depths 1–3 from the start position (**[ran]** passes, about 40k nodes/s interpreted). That depth does not reach castling, en passant or promotion. Add depths 4–5 and the standard Kiwipete and endgame positions.
- [ ] No draw by repetition or insufficient material, so bot-versus-bot games can shuffle until the 50-move rule.
- [ ] The move-ordering comparator calls `move-value` on every comparison; score once and sort the pairs.
- [ ] Mate-score adjustment uses the global `max-depth`, which changes during iterative deepening; store ply-relative mate scores.
- [ ] `logand`/`logior`/`logxor`/`lognot` aliases are copied into several files; export them from `engine.ss`.
- [ ] The Makefile runs everything under `gxi`. Add an optimized `gxc -O -exe` target and report the speedup, a good demonstration of Gerbil's compiler.
- [ ] Add time-based search control, and consider a minimal UCI loop so the engine can play in a GUI.
- [ ] At 6,900 words with full listings this is the longest chapter; consider splitting engine and search.

### Daily-use assistant (`Daily_use_AI_assistant.md`)

- [ ] `cache-lookup` returns entries matching *any* keyword, in insertion order. Rank by number of matching keywords. The chapter calls this RAG; say plainly that it is keyword recall.
- [ ] The cache file is rewritten in full on every add and is named `.db` although it is s-expressions; `print-help` hardcodes its path.
- [ ] The REPL handles `!` in two branches with duplicated code.
- [ ] `string-split` builds words by repeated `string-append`.
- [ ] `manifest.ss` and `build-deps` are committed here and gitignored in the other `gxpkg` projects.
- [ ] The sample session includes "Sunday, July 9, 2026"; that date was a Thursday. Regenerate, or use it to make a point about model errors.
- [ ] Uses "Practice Problems" as the heading; other chapters use "Optional Practice Problems" (also `chess-game.md`).

### Reinforcement learning (`ReinforcementLearning.md`)

- [ ] `q-learning` hardcodes 16 states and 4 actions although the environment carries `n-states`/`n-actions`.
- [ ] `frozen-lake-step!` returns an alist that callers take apart with three `assoc` calls; return multiple values or a struct.
- [ ] Episodes are cut at 100 steps silently; state it in the text.

### Web scraping (`WebScraping.md`)

- [ ] Each API function fetches the page itself, so the test makes four requests to the same URL. Split fetching from parsing (`html->text`, `html->markdown`, `html->links`, `html->headers`) so the parsers can be tested offline against a saved fixture.
- [ ] `get-links` runs three patterns and appends the results, so links are not in document order. Relative URLs are not resolved.
- [ ] Block-level tags are matched only without attributes (`</?[pP]>`, `</?[dD][iI][vV]>`), so `<div class="...">` does not produce a line break.
- [ ] The `[sS][cC][rR]...` character classes can be replaced with pregexp's case-insensitive `(?i:...)`.
- [ ] The client sends a spoofed Chrome User-Agent. Use an honest one and add a paragraph on robots.txt, rate limiting and terms of use.
- [ ] The test depends on the live content of markwatson.com.

---

## 5. Book structure and consistency

- [ ] **`manuscript/TOC.md` is stale.** It lists 10 of the 20 chapters in `Book.txt`, has "CHapter 10" with a stray quote, and says "This book is a work in progress". The README sends readers there. Regenerate it from `Book.txt`.
- [ ] **Title and URL disagree across files.** The README says "Gerbil Scheme in Action: AI Applications, Network Programming, and Utilities"; `source_code/AGENT_SKILLS_README.md` says "Artificial Intelligence Algorithms in Gerbil Scheme" and clones `github.com/markwatson/...`, while the preface uses `github.com/mark-watson/...`.
- [ ] **`manuscript/Subset.txt`** contains only `kmeans.md`; confirm that is intended.
- [ ] **`command_rag.md`** is a 140-word stub outside `Book.txt` with typos ("Chating", "efficient;y", "docments"). Finish it or remove it. `source_code/autocontext` (BM25 and TF-IDF retrieval) is ready-made material for it.
- [ ] **Chapter order.** Current order is LLM clients, Wikidata, NLP, FFI ×2, WebKit, RDF store, CLI ×2, ML ×2, chess, assistant, RL, web scraping. Suggested parts: (1) setup and language primer, command-line utilities; (2) LLM clients, daily-use assistant, web scraping; (3) Wikidata, in-memory RDF store, NLP; (4) FFI: Raptor, Rasqal, WebKit; (5) anomaly detection, K-means, RL, chess. This fixes forward references such as "as we saw in an earlier chapter" in `RDF_simple.md` and `anomaly-detection.md` announcing "the next chapter".
- [ ] **Architecture diagrams** exist for the first 13 chapters only. Add them for anomaly detection, K-means, chess, the assistant, RL and web scraping, or drop the convention. `source_code/.gitignore` ignores the `*_architecture.png` names, and each project also holds its own `architecture.png` (about 650 KB each); keep one copy.
- [ ] **Practice problems** exist for every chapter but have no solutions. Add a `solutions/` directory or an appendix for at least one problem per chapter.
- [ ] **Repeated typo** "an run" in `gemini.md`, `ollama.md`, `ffi_wrapper.md`, `chess-game.md`, `Daily_use_AI_assistant.md`.
- [ ] **Code-fence language tags** vary: `{lang="scheme", linenos=off}` with a bare fence in early chapters, tagged backtick fences later, tilde fences in `webkit.md`. Standardize.
- [ ] **`SOURCE_CODE_MAP.md`** omits `hello` and the six `NOT_IN_BOOK` projects. List them with their status.
- [ ] **`GEMINI.md`** tells an agent to use a `gerbil-expert` skill; add the equivalent note for other coding agents, or fold it into `AGENT_SKILLS_README.md`.

## 6. Candidate new chapters from existing code

Six projects carry `NOT_IN_BOOK.md` and already have READMEs:

- [ ] `autocontext` — hybrid BM25/TF-IDF context retrieval. Natural replacement for the `command_rag.md` stub and an upgrade for the assistant's cache lookup.
- [ ] `coding-harness` — the largest project (about 3,000 lines, with a paren checker and approval flow). A chapter on building an LLM coding agent would be the strongest addition to the LLM part.
- [ ] `microGPT` — a tiny GPT from scratch; pairs with the ML chapters.
- [ ] `Probability` — Bayesian and frequentist reasoning; fits before anomaly detection.
- [ ] `symbolic-math` — polynomial differentiation and integration; a classic Lisp topic and a good vehicle for `match`.
- [ ] `text-adventure-game` — LLM-driven game; a lighter closing chapter.
