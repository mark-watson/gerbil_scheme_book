# Gerbil Coding Harness

An AI coding agent for **Gerbil Scheme** projects, written in Gerbil Scheme. You run it in a project directory and it works through a multi-turn tool-calling loop. It shows every edit as a colored diff and waits for your approval. After each accepted edit it runs `make check`, and **syntax errors, mismatched parens above all, come back to you to fix instead of going back to the model.**

The design follows the general-purpose Racket harness in *Practical Artificial Intelligence Development With Racket* (`Racket-AI-book/source-code/coding-agent-harness`), but this version is built around Gerbil:

| | General-purpose Racket harness | Gerbil harness |
|---|---|---|
| System prompt | generic coding rules | Gerbil idioms and pitfalls, plus the **gerbil-expert skill** loaded by default |
| Paren problems | whatever the compiler says | own reader-accurate scanner: **line, column, excerpt, likely fix** |
| After `make check` fails | output goes to the model | **syntax errors go to you**; other failures go to the model |
| Unbalanced proposed code | not detected | warning shown with the diff; `-y` never writes it |
| No Makefile | `make check` fails | built-in check: paren scan + `gxc -S` |
| Extra tools | | `check_parens`, `outline_file`, `find_definition`, `search_examples`, `gxi_eval` |
| Intent classifier, web search | yes | removed (every request is treated as a Gerbil coding task) |

## Why syntax errors go to the user

When `gxc` cannot read a file, it gives no position at all:

```
--- Syntax Error at read-module: error reading module
... detail: "Incomplete form, EOF reached\n"
```

Language models are bad at counting parentheses. Given an error like that, a model typically rewrites the whole file and moves the imbalance somewhere else. A person with an editor that highlights matching parens fixes it in seconds, as long as someone says *where* the problem is. The harness does that part:

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

Please fix this syntax error in your editor (the harness does not let the model guess at paren fixes).
  [Enter]  I fixed it: re-run the check
  r        revert this edit
  m        hand the diagnostics to the model and let it try
  a        abort: leave the file as it is and stop this task
fix>
```

Press **Enter** after you fix the file and the check runs again, as many times as you need. When it passes, the model is told that you edited the file by hand, so it reads the file again before changing it. You can also type **r** to undo the edit, or **m** to let the model try anyway.

## The edit flow

```
model calls propose_edit(path, old, new)
  │
  ├─ stale `old`? ──────────────────────────────► "stale base, re-read the file"
  ├─ paren-scan `new` (Gerbil sources)
  ├─ show colored diff  (+ yellow warning and diagnostics if unbalanced)
  ├─ ask:  y / n / s(kip + reason) / p(send paren errors back to model)
  │
  └─ y: write file, run `make check`
          ├─ passes ─────────────────────────────► "applied; make check passed"
          ├─ syntax error (reader or expander) ──► YOU fix → Enter → re-check …
          └─ other failure ─────────────────────► output + diagnostics to the model
```

Syntax errors are detected in two ways. The harness re-scans the edited file and every file named in the check output, and it matches Gerbil's reader and expander messages (`Incomplete form`, `Datum or EOF expected`, `Syntax Error ...`). Expander errors such as an unbound identifier include a position (`"foo.ss"@12.3`), and the harness turns it into a source excerpt with a caret.

By default every syntax error goes to you. To send only unreadable files and unbalanced parens to you, and let the model handle expander errors like unbound identifiers, set `"ask_user_on": "reader"`.

## The delimiter scanner (`parens.ss`)

The scanner reads source the way the Gerbil reader does, so it does not get confused by these:

- strings with escapes, including strings that span lines, and `|symbols with (parens)|`
- `;` line comments and nested `#| ... |#` block comments
- character literals: `#\(`, `#\)`, `#\"`, `#\;`, `#\space`, `#\x41`
- vectors `#(...)`, Gerbil list brackets `[...]`, method-call braces `{...}`
- Gambit here-strings `#<<END ... END`

It reports four kinds of problem:

- an unexpected closer
- a mismatched closer, for example `]` where `)` was expected, with both positions
- an opener that is never closed, together with the string of closers that would balance it
- an unterminated string or comment

Two heuristics point at where the real mistake usually is. A form that starts in column 1 while an earlier top-level form is still open almost always means a missing `)` just before it. For an unterminated string, the scanner lists the lines that have an odd number of `"` characters.

I ran the scanner over the 87 `.ss` files in this book's `source_code/`. 86 came back clean. The one it flagged has a real missing quote: line 14 of `SparqlRdfStore/test.ss` reads `(prog DEMO_rdfwrap")`.

## Requirements

- Gerbil Scheme v0.18+ (`gxi`, `gxc` on your PATH)
- `diff`, `grep` (standard on macOS/Linux)
- An OpenAI-compatible chat-completions endpoint: Fireworks, DeepSeek, OpenAI, a local `mlx_lm.server`, oMLX, or Ollama's `/v1` shim
- Optional: `rlwrap` for line editing and history (`brew install rlwrap`)
- Optional: the gerbil-expert skill at `~/.agents/skills/gerbil-expert/SKILL.md`

## Setup

```bash
cd gerbil-coding-harness
make check            # expand-compiles every module and runs the tests
make install          # writes a `gerbil-harness` launcher to /usr/local/bin
                      # (PREFIX=~/.local to install elsewhere)
```

The launcher is a two-line shell script that runs `gxi harness.ss` under `rlwrap` when rlwrap is installed. A standalone binary built with `gxc -exe` would also work. On Homebrew Gerbil, though, linking can fail with `library 'ssl' not found` because the OpenSSL path recorded at build time no longer exists. Running through `gxi` avoids that problem.

## Configuration

The harness uses the same two JSON layers as the Racket and Python harnesses, so one config serves all three:

- **Global:** `~/.coding_harness.json`
- **Local:** `.local_coding_harness.json` in the project directory. It overrides the global file, and nested objects are merged.

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
  "gerbil": {
    "skill": "gerbil-expert",
    "autoload_skill": true,
    "examples_dir": "~/GITHUB/gerbil_scheme_book/source_code",
    "check_command": "make check",
    "ask_user_on": "syntax",
    "max_iterations": 30,
    "eval_timeout": 20
  }
}
```

Every section of `gerbil` is optional. The values shown above are the defaults.

| Key | Meaning |
|---|---|
| `skill`, `autoload_skill` | Skill file appended to the system prompt (about 18 KB for gerbil-expert). Turn it off with `false` or `--no-skill` for small local models. |
| `examples_dir` | Directory searched by `search_examples` and `/examples` |
| `check_command` | Command run after each accepted edit. If it starts with `make` and the project has no Makefile or no `check` target, the built-in check runs instead. |
| `ask_user_on` | `"syntax"`: every syntax error goes to you. `"reader"`: only unbalanced or unreadable files go to you. |
| `max_iterations` | Tool-calling rounds per request |
| `eval_timeout` | Seconds before a `gxi_eval` process is killed |

All providers use the OpenAI chat-completions format, and the `type` field is accepted for compatibility. No provider settings are compiled into the code.

## Usage

```bash
gerbil-harness                                   # REPL (or: make run, gxi harness.ss)
gerbil-harness "add a test for parse-row"        # one-shot
gerbil-harness -y "rename foo to bar in util.ss" # auto-approve edits (diff still shown)
gerbil-harness --dry-run "refactor main.ss"      # show diffs, write nothing
gerbil-harness --provider omlx --no-skill "..."  # local model, lean prompt
echo "explain checker.ss" | gerbil-harness --stdin
```

| Flag | |
|---|---|
| `-p, --prompt TEXT` | prompt text (repeatable) |
| `--stdin` | read the prompt from stdin |
| `-y, --yes` | auto-approve edits and `gxi_eval` (unbalanced code is still never auto-written) |
| `--dry-run` | show diffs, never write |
| `--provider NAME`, `--model ID` | pick a provider profile and model |
| `--cwd DIR` | work in DIR (its `.local_coding_harness.json` is used) |
| `--no-skill` | leave the gerbil-expert skill out of the system prompt |
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

When stdin is not a terminal, for example with `--stdin` or in CI, the harness never stops to ask a question. Edits are not applied unless you pass `-y`, and syntax errors go to the model with full diagnostics.

## Tools the model can call

| Tool | |
|---|---|
| `read_file`, `list_dir`, `grep` | Explore the project. Dotfiles, editor backups and `.gerbil/` are skipped. |
| `outline_file` | Top-level forms of a `.ss` file with line ranges. Cheaper than reading a big file. |
| `find_definition` | Where an identifier is defined (`def`, `defstruct`, `defclass`, `defrule`, `defsyntax`, `defmethod`, ...) |
| `check_parens` | Run the delimiter scanner on a file |
| `search_examples` | Grep the Gerbil book examples for working usage of a `:std` API |
| `gxi_eval` | Evaluate forms in a fresh `gxi` in the project directory (`(import "foo")` works). The value of the last expression is printed. You approve each run. It has a timeout. |
| `run_shell` | Whitelisted commands only: make, ls, pwd, cat, head, tail, wc, diff, gxc, gxpkg, gerbil |
| `propose_edit` | The only tool that writes files. Shows the diff, asks for approval, then runs the check (see above). |

## REPL commands

| Command | |
|---|---|
| `/check` | Run the check now. A syntax error triggers the same fix-and-press-Enter loop. |
| `/parens <file>` | Delimiter report for a file |
| `/outline <file>` | Top-level forms of a file |
| `/eval <code>` | Evaluate Gerbil code with gxi, no approval prompt |
| `/examples <regex>` | Search the book examples |
| `/yes` | Toggle auto-approve |
| `/reset`, `/history`, `/context`, `/compact` | Manage the conversation |
| `/model <id>`, `/provider [name]`, `/tokens`, `/debug` | Model, provider, cost and logging |
| `/skills`, `/<skill-name>` | List skills or load one into the conversation |
| `/quit` | Exit |

## Project layout

```
harness.ss         main: Gerbil system prompt, REPL, slash commands, CLI
tools.ss           tool registry; propose_edit; Gerbil tools
checker.ss         make-check gate, syntax-error classification, user fix loop
parens.ss          reader-accurate delimiter scanner, excerpts, top-level forms
llm.ss             OpenAI-compatible client, tool-calling loop, token/cost stats
approval.ss        colored diffs, questions to the user
config.ss          hierarchical JSON config, "gerbil" section, session flags
util.ss            subprocesses (with timeout), colors, files
test/parens-test.ss  scanner and classifier tests (make test)
```

## License

AGPL-3.0. Copyright (C) 2026 Mark Watson. See [LICENSE](LICENSE).
