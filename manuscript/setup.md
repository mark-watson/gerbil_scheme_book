# Setting Up Gerbil Scheme Development Environment

We covered the basic installation of Gerbil Scheme for macOS and Linux in the Preface. In this chapter, we verify your installation with a minimal first program, explain the Gerbil compilation and package workflow, configure editor support in Emacs and Visual Studio Code, and review the essential language features that Gerbil adds to standard Scheme.

## Verifying Your Installation with Hello World

Before writing complex applications, verify that your Gerbil environment is working properly. The repository includes a minimal first example in `source_code/hello/hello.ss`.

Here is the complete source code of `hello.ss`:

```scheme
(def (hello who)
  (displayln "hello " who))

(hello 'Brady)
```

In standard Scheme, one writes `define` and calls both `display` and `newline`. Gerbil provides `def` as a concise shorthand and includes `displayln` in its standard library.

### Interpreted Execution

You can run this program immediately using the Gerbil interpreter, `gxi`:

```bash
$ cd source_code/hello
$ gxi hello.ss
hello Brady
```

### Interactive REPL Session

You can also load the file into an interactive REPL session:

```bash
$ gxi
Gerbil v0.18.1 on Gambit v4.9.5
> (import "hello")
> (hello 'World)
hello World
> ,q
```

### Compiling to a Standalone Executable

Gerbil uses Gambit Scheme as its backend compiler. You can compile Scheme source directly to an optimized native binary using `gxc`:

```bash
$ gxc -exe -o hello hello.ss
$ ./hello
hello Brady
```

This compilation step translates Gerbil Scheme into Gambit intermediate code, compiles to C, and links into a standalone executable.

## The Gerbil Workflow: Modules, Packages, and Build Tools

As your projects grow beyond single scripts, Gerbil organizes code into packages and modules.

### Packages and `gerbil.pkg`

A Gerbil package is a directory containing a `gerbil.pkg` manifest. The manifest declares the package symbol name and any dependencies:

```scheme
(package: myproject)
```

When a package defines `(package: myproject)`, a source file named `utils.ss` inside that directory is addressed by other modules as `:myproject/utils`.

### Build Scripts: `build.ss` and `gerbil build`

Standard packages include a `build.ss` script specifying the compilation targets:

```scheme
#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '("utils"
    (exe: "main" bin: "mytool")))
```

Running `gerbil build` compiles library modules into compiled bytecode archives (`.ssi` interface files and `.o1` object files) and builds any declared executables into `bin/`.

### Where Compiled Modules Live: `GERBIL_PATH`

By default, `gerbil build` and `gxc` install compiled modules into `~/.gerbil/lib`. During development, you can isolate project dependencies by pointing the `GERBIL_PATH` environment variable to a local directory:

```bash
export GERBIL_PATH=$(pwd)/.gerbil
```

When `GERBIL_PATH` is set, Gerbil searches for libraries and writes compilation artifacts within that local directory rather than altering your user-wide environment.

## Editor Setup

### Emacs Configuration

Emacs provides a mature development environment for Gerbil through `gerbil-mode` and Gambit integration.

Download `gambit.el` and `gerbil-mode.el` into `~/.emacs.d/`:

- [https://github.com/gambit/gambit/blob/master/misc/gambit.el](https://github.com/gambit/gambit/blob/master/misc/gambit.el)
- [https://github.com/mighty-gerbils/gerbil/blob/master/etc/gerbil-mode.el](https://github.com/mighty-gerbils/gerbil/blob/master/etc/gerbil-mode.el)

Then add the following configuration to your `~/.emacs` or `~/.emacs.d/init.el`:

```lisp
(load "~/.emacs.d/gambit.el")
(load "~/.emacs.d/gerbil-mode.el")

(autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
(defvar gerbil-program-name
  (expand-file-name "/opt/gerbil/bin/gxi")) ; adjust to match which gxi
(setq scheme-program-name gerbil-program-name)
```

With this configuration, opening any `.ss` file activates `gerbil-mode`, providing syntax highlighting, automatic indentation, and direct expression evaluation with `C-x C-e`.

### Visual Studio Code

If you prefer Visual Studio Code:

1. Install the **vscode-scheme** or **Gerbil** extension from the VS Code Marketplace.
2. In your VS Code settings, set the Scheme interpreter path to your `gxi` executable:
   ```json
   {
     "scheme.interpreterPath": "/usr/local/bin/gxi"
   }
   ```
3. Use the integrated terminal to run `gxi` REPL sessions and `gerbil build` tasks.

## Gerbil for Schemers: Quick Language Primer

If you are already familiar with standard Scheme (R5RS or R7RS), Gerbil introduces several powerful syntactic extensions and library conveniences that appear throughout this book.

### Concise Definitions with `def`

Gerbil uses `def` for definitions:

```scheme
(def x 42)
(def (add a b) (+ a b))
```

### Keyword and Optional Arguments

Functions support optional arguments with default values and keyword arguments:

```scheme
(def (greet name (greeting "Hello") punctuation: (punct "!"))
  (string-append greeting ", " name punct))

(greet "Alice")                           ; => "Hello, Alice!"
(greet "Bob" "Hi")                        ; => "Hi, Bob!"
(greet "Carol" punctuation: "?")          ; => "Hello, Carol?"
```

### Records and Data Structures with `defstruct`

Gerbil provides typed structs through `defstruct`:

```scheme
(defstruct point (x y))

(def p (make-point 10 20))
(point-x p) ; => 10
(point-y p) ; => 20
(point? p)  ; => #t
```

### Pattern Matching with `match`

The `:std/sugar` module provides expressive pattern matching:

```scheme
(import :std/sugar)

(def (eval-expr expr)
  (match expr
    (['+ a b] (+ (eval-expr a) (eval-expr b)))
    (['* a b] (* (eval-expr a) (eval-expr b)))
    ((? number? n) n)
    (else (error "Unknown expression" expr))))
```

### Exception Handling with `try` and `with-catch`

Gerbil provides structured exception handling:

```scheme
(try
  (open-input-file "missing.txt")
  (catch (e)
    (displayln "Handled error: " (error-message e))))
```

### Hash Tables

Hash tables are first-class with convenient constructors:

```scheme
(def h (make-hash-table))
(hash-put! h 'key "value")
(hash-ref h 'key)          ; => "value"
(hash-ref h 'missing "default") ; => "default"
```

### Iteration with `for`

The `:std/iter` module provides comprehension and looping constructs:

```scheme
(import :std/iter)

(for (x '(1 2 3 4 5))
  (displayln (* x x)))
```

With these fundamentals established, you are ready to begin building applications with Gerbil Scheme.
