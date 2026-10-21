# Coding Harness

An AI coding agent for **Gerbil Scheme, Python, Haskell, TypeScript and JavaScript** projects, written in Gerbil Scheme. Other languages work too, with less help. You run it in a project directory and it works through a multi-turn tool-calling loop. It shows every edit as a colored diff and waits for your approval. After each accepted edit it runs the project check (`make check` by default). **Syntax errors, unbalanced delimiters above all, come back to you to fix instead of going back to the model.**

The design follows the general-purpose Racket harness in *Practical Artificial Intelligence Development With Racket* (`Racket-AI-book/source-code/coding-agent-harness`). It started as a Gerbil-only harness; a language profile (`lang.ss`) now supplies everything language-specific:

| | What the profile supplies |
|---|---|
| System prompt | the language's idioms and pitfalls, plus an optional skill (gerbil-expert for Gerbil) |
| Delimiter scanner | how to skip strings and comments, so it can report unbalanced `()[]{}` with **line, column, excerpt, likely fix** |
| Built-in check (no Makefile) | `gxc -S`, Python's `ast.parse`, `ghc -fno-code` or `cabal build`, `tsc --noEmit`, `node --check` |
| Syntax-error rules | which compiler messages count as unreadable source and which as other syntax errors |
| `find_definition`, `outline_file` | grep patterns for the language's definitions |
| `eval_code` | `gxi`, `python3`, `ghc -e`, `node -p` |
| `run_shell` whitelist | the language's build, test and lint commands |

## Languages

| Language | Detected by | Built-in check | `eval_code` |
|---|---|---|---|
| `gerbil` | `gerbil.pkg`, `.ss` files | delimiter scan + `gxc -S` | `gxi -e`, value of the last form printed |
| `python` | `pyproject.toml`, `setup.py`, `requirements.txt`, ... | delimiter scan + `ast.parse` (no `.pyc` written) | value of the last expression printed; uses `.venv/bin/python` when present |
| `haskell` | `*.cabal`, `stack.yaml`, `package.yaml` | delimiter scan + `cabal build` / `stack build`, or `ghc -fno-code` per file | declarations then one expression, or a program with `main` |
| `typescript` | `tsconfig.json`, `.ts` files | delimiter scan + `tsc --noEmit` when `tsc` is installed | `node -p` (Node 23.6+ strips types) |
| `javascript` | `package.json`, `.js` files | delimiter scan + `node --check` | `node -p` |
| `generic` | nothing else matched | your `check_command` only | none |

The harness picks the project language from `--lang`, then `"language"` in the config, then marker files, then the most common source extension in `.`, `src/`, `lib/`, `app/` and `test(s)/`. Type `/lang python` in the REPL to switch.

Per-file work goes by each file's extension, whatever the project language. In a Python project with a Haskell helper, the harness scans and checks the `.hs` file as Haskell.

## Why syntax errors go to the user

When `gxc` cannot read a file, it gives no position at all:

```
--- Syntax Error at read-module: error reading module
... detail: "Incomplete form, EOF reached\n"
```

Language models are bad at counting parentheses. Given an error like that, a model typically rewrites the whole file and moves the imbalance somewhere else. A person with an editor that highlights matching parens fixes it in seconds, once someone says *where* the problem is. The harness does that part:

```
━━ SYNTAX ERROR: `make check` failed (unbalanced/unreadable) ━━
m.ss: UNBALANCED DELIMITERS

* '(' opened at line 3, col 1 ((def (add2 x)) is never closed: EOF reached with
  1 delimiter(s) still open; the minimal fix appends ")" -- but check the hint
      2| (def (add1* x) (+ x 1))
      3| (def (add2 x)
         ^
      4|   (let ((y (add1* x)))

hint: line 9 starts a new form in column 1 ((def (main . args)) while the form
      at line 3 ((def (add2 x)) is still open -- it is probably missing 1
      closing delimiter(s) before line 9

Please fix this syntax error in your editor (the harness does not let the model guess at delimiter fixes).
  [Enter]  I fixed it: re-run the check
  r        revert this edit
  m        hand the diagnostics to the model and let it try
  a        abort: leave the file as it is and stop this task
fix>
```

Press **Enter** after you fix the file and the check runs again, as many times as you need. When it passes, the harness tells the model you edited the file by hand, so it reads the file again before changing it. You can also type **r** to undo the edit, or **m** to let the model try anyway.

Models write Python, Haskell and TypeScript with fewer bracket slips than Lisp, so those languages default to `"ask_user_on": "reader"`: only unbalanced or unreadable files come to you, and the model fixes other syntax errors, such as a bad indent. Gerbil defaults to `"syntax"`. Set `"never"` to send everything to the model.

## The edit flow

```
model calls propose_edit(path, old, new)
  │
  ├─ stale `old`? ──────────────────────────────► "stale base, re-read the file"
  ├─ delimiter-scan `new` (by file extension)
  ├─ show colored diff  (+ yellow warning and diagnostics if unbalanced)
  ├─ ask:  y / n / s(kip + reason) / p(send delimiter errors back to model)
  │
  └─ y: write file, run the check
          ├─ passes ─────────────────────────────► "applied; check passed"
          ├─ syntax error (per ask_user_on) ─────► YOU fix → Enter → re-check …
          └─ other failure ─────────────────────► output + diagnostics to the model
```

The harness finds syntax errors in two ways. It re-scans the edited file and every project file named in the check output, and it matches the language's parser messages (`Incomplete form`, `SyntaxError`, `parse error`, `error TS1005`, ...). It also reads positions out of the output (`"foo.ss"@12.3`, `File "x.py", line 12`, `Foo.hs:12:3`, `a.ts(12,3)`) and turns each one into a source excerpt with a caret. It skips positions in files outside the project, such as standard-library frames in a Python traceback.

With `-y`, the harness never writes unbalanced Gerbil code. For other languages it shows the warning and writes the file, and the check after the write catches a real imbalance.

## The delimiter scanner (`parens.ss`)

The scanner reads source the way the language's reader does. Each language has a lexicon:

| Lexicon | Used for | Skips |
|---|---|---|
| `lisp` | Gerbil, Scheme | strings with escapes, `\|symbols (with parens)\|`, `;` and nested `#\| \|#` comments, `#\(` and other char literals, `#<<END` here-strings; knows `#(`, `[...]`, `{...}` |
| `python` | Python | `#` comments, `'...'`, `"..."`, `'''...'''`, `"""..."""` |
| `haskell` | Haskell | `--` comments (but not the `-->` operator), nested `{- -}` comments, `"..."`, `'c'` (but not the prime in `x'`) |
| `c-like` | TypeScript, JavaScript | `//` and `/* */` comments, `'...'`, `"..."`, template literals with code in `${...}`, `/regex/` literals |

It skips `.tsx` and `.jsx` files, since JSX text is not code.

It reports four kinds of problem:

- an unexpected closer
- a mismatched closer, for example `]` where `)` was expected, with both positions
- an opener that is never closed, together with the string of closers that would balance it
- an unterminated string or comment (for Python, JavaScript and Haskell, a one-line string that reaches the end of its line)

Two heuristics point at where the real mistake usually is. A line that starts in column 1 while an earlier bracket is still open almost always means a missing closer just before it. For an unterminated Lisp string, the scanner lists the lines with an odd number of `"` characters.

Results on real code:

| Files | Flagged | |
|---|---|---|
| 87 Gerbil (`source_code/` of this book) | 1 | a real missing quote: line 14 of `SparqlRdfStore/test.ss` reads `(prog DEMO_rdfwrap")` |
| 669 Python | 0 | |
| 146 Haskell | 0 | |
| 1,500 TypeScript | 0 | |
| 3,198 JavaScript (npm's own modules, minified bundles included) | 0 | |

## Requirements

- Gerbil Scheme v0.18+ (`gxi`, `gxc` on your PATH) to run the harness
- `diff`, `grep` (standard on macOS/Linux)
- An OpenAI-compatible chat-completions endpoint: Fireworks, DeepSeek, OpenAI, a local `mlx_lm.server`, oMLX, or Ollama's `/v1` shim
- For each language you use: its tools (`python3`; `ghc` and `cabal` or `stack`; `node`; `tsc` for full TypeScript checks)
- Optional: `rlwrap` for line editing and history (`brew install rlwrap`)
- Optional: skills in `~/.agents/skills/<name>/SKILL.md`, such as gerbil-expert

## Setup

```bash
cd coding-harness
make check            # expand-compiles every module and runs the tests
make install          # writes a `gerbil-harness` launcher to ~/bin
                      # (PREFIX=/usr/local to install elsewhere,
                      #  NAME=coding-harness to rename the launcher)
```

The launcher is a two-line shell script that runs `gxi harness.ss` under `rlwrap` when rlwrap is installed. A standalone binary built with `gxc -exe` would also work. On Homebrew Gerbil, though, linking can fail with `library 'ssl' not found` because the OpenSSL path recorded at build time no longer exists. Running through `gxi` avoids that problem.

## Configuration

The harness uses the same two JSON layers as the Racket and Python harnesses, so one config serves all three:

- **Global:** `~/.coding_harness.json`
- **Local:** `.local_coding_harness.json` in the project directory. It overrides the global file, and nested objects merge.

```json
{
  "default_provider": "deepseek",
  "providers": {
    "deepseek": {
      "type": "openai",
      "endpoint": "https://api.deepseek.com/v1/chat/completions",
      "api_key_env": "DEEPSEEK_API_KEY",
      "model": "deepseek-flash",
      "generation": { "temperature": 0.6, "max_tokens": 32768 },
      "pricing": { "input": 0.15, "cached_input": 0.003, "output": 0.6 }
    },
    "omlx": {
      "type": "mlx",
      "endpoint": "http://127.0.0.1:8000/v1/chat/completions",
      "model": "mlx-community--Qwen3.6-35B-A3B-4bit",
      "generation": { "temperature": 0.6, "max_tokens": 32768 }
    }
  },
  "language": "python",
  "harness": {
    "check_command": "make check",
    "max_iterations": 30,
    "eval_timeout": 20
  },
  "languages": {
    "python": { "check_command": "pytest -q", "skill": "python-dev" },
    "gerbil": { "examples_dir": "~/GITHUB/gerbil_scheme_book/source_code" }
  }
}
```

Every key besides `providers` is optional. The harness looks a setting up in `languages.<project language>`, then `harness`, then the language's defaults, then the harness defaults. `"language"` belongs in a project's local file, if anywhere; leave it out to let the harness detect the language. An older top-level `"gerbil"` section still works and counts as `languages.gerbil`.

| Key | Default | Meaning |
|---|---|---|
| `check_command` | `"make check"` | Command run after each accepted edit. If it starts with `make` and the project has no Makefile or no `check` target, the built-in check runs instead. |
| `ask_user_on` | `"syntax"` for Gerbil, `"reader"` otherwise | `"syntax"`: every syntax error goes to you. `"reader"`: only unbalanced or unreadable files. `"never"`: all go to the model. |
| `skill`, `autoload_skill` | `"gerbil-expert"` for Gerbil, none otherwise; `true` | Skill file appended to the system prompt (about 18 KB for gerbil-expert). Turn it off with `false` or `--no-skill` for small local models. |
| `examples_dir` | the Gerbil book's `source_code` for Gerbil, none otherwise | Directory searched by `search_examples` and `/examples`. The tool only appears when this directory exists. |
| `max_iterations` | `30` | Tool-calling rounds per request |
| `eval_timeout` | `20` | Seconds before an `eval_code` process is killed |

All providers use the OpenAI chat-completions format, and the harness accepts the `type` field for compatibility. The code holds no provider settings.

## Usage

```bash
gerbil-harness                                   # REPL (or: make run, gxi harness.ss)
gerbil-harness "add a test for parse_row"        # one-shot
gerbil-harness --lang haskell "add a Show instance for Shape"
gerbil-harness -y "rename foo to bar in util.py" # auto-approve edits (diff still shown)
gerbil-harness --dry-run "refactor main.ts"      # show diffs, write nothing
gerbil-harness --provider omlx --no-skill "..."  # local model, lean prompt
echo "explain checker.ss" | gerbil-harness --stdin
```

| Flag | |
|---|---|
| `-p, --prompt TEXT` | prompt text (repeatable) |
| `--stdin` | read the prompt from stdin |
| `-y, --yes` | auto-approve edits and `eval_code` (unbalanced Gerbil code is still never auto-written) |
| `--dry-run` | show diffs, never write |
| `--provider NAME`, `--model ID` | pick a provider profile and model |
| `--cwd DIR` | work in DIR (its `.local_coding_harness.json` is used) |
| `--lang NAME` | project language: `gerbil`, `python`, `haskell`, `typescript`, `javascript`, `generic` |
| `--no-skill` | leave the language's skill out of the system prompt |
| `--debug`, `-q/--quiet`, `--plain/--no-color` | |

Exit codes for scripting:

| Code | Meaning |
|---|---|
| `0` | ok |
| `1` | model or network error |
| `2` | check failed |
| `3` | edit rejected or skipped |
| `4` | syntax error left unresolved |
| `5` | bad arguments |

When stdin is not a terminal, for example with `--stdin` or in CI, the harness never stops to ask a question. It applies no edits unless you pass `-y`, and syntax errors go to the model with full diagnostics.

## Tools the model can call

| Tool | |
|---|---|
| `read_file`, `list_dir`, `grep` | Explore the project. Skips dotfiles, editor backups, `node_modules`, virtualenvs and build output (`.gerbil/`, `dist-newstyle/`, `.stack-work/`, ...). |
| `outline_file` | Top-level forms of a Lisp file with line ranges; functions, classes and types of other languages with line numbers. Cheaper than reading a big file. |
| `find_definition` | Where an identifier is defined, using the language's definition forms (`def`/`defstruct`, `def`/`class`, `name ::`/`data`, `function`/`interface`/`const`, ...) |
| `check_delimiters` | Run the delimiter scanner on a file |
| `search_examples` | Grep `examples_dir` for working usage of an API (only when `examples_dir` is set) |
| `eval_code` | Run a snippet in a fresh process in the project directory, so local imports work. You approve each run. It has a timeout. Not offered for `generic`. |
| `run_shell` | Whitelisted command prefixes only. Always: `make`, `ls`, `pwd`, `cat`, `head`, `tail`, `wc`, `diff`. Plus Gerbil: `gxc`, `gxpkg`, `gerbil`; Python: `pytest`, `ruff check`, `mypy`, `pyright`; Haskell: `cabal build/test`, `stack build/test`, `hlint`, `ghc -fno-code`; TS/JS: `tsc`, `npm test`, `npm run`, `npx tsc/eslint/jest`, `npx vitest run`, `node --check` |
| `propose_edit` | The only tool that writes files. Shows the diff, asks for approval, then runs the check (see above). |

## REPL commands

| Command | |
|---|---|
| `/check` | Run the check now. A syntax error triggers the same fix-and-press-Enter loop. |
| `/delims <file>` | Delimiter report for a file (`/parens` also works) |
| `/outline <file>` | Definitions in a file |
| `/eval <code>` | Run code in the project language, no approval prompt |
| `/examples <regex>` | Search `examples_dir` |
| `/lang [name]` | Show or switch the project language (keeps the conversation, swaps the system prompt) |
| `/yes` | Toggle auto-approve |
| `/reset`, `/history`, `/context`, `/compact` | Manage the conversation |
| `/model <id>`, `/provider [name]`, `/tokens`, `/debug` | Model, provider, cost and logging |
| `/skills`, `/<skill-name>` | List skills or load one into the conversation |
| `/quit` | Exit |

## Adding a language

Add a `make-language` profile to `lang.ss` and put it in `all-languages`. If the language's comments and strings look like C's, use the `c-like` lexicon; otherwise add a lexer to `LEXERS` in `parens.ss`, or use `#f` to skip delimiter scans. Add marker files to `MARKERS` so the harness detects the language.

## Project layout

```
harness.ss         main: system prompt, REPL, slash commands, CLI
lang.ss            language profiles, language detection, eval commands
tools.ss           tool registry; propose_edit; code tools
checker.ss         check gate, built-in check, syntax-error classification, user fix loop
parens.ss          delimiter scanner (lisp, python, haskell, c-like lexicons), excerpts
llm.ss             OpenAI-compatible client, tool-calling loop, token/cost stats
approval.ss        colored diffs, questions to the user
config.ss          hierarchical JSON config, per-language settings, session flags
util.ss            subprocesses (with timeout), colors, files
test/parens-test.ss  scanner, language and classifier tests (make test)
```

## License

AGPL-3.0. Copyright (C) 2026 Mark Watson. See [LICENSE](LICENSE).
