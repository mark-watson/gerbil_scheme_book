# webkit-gerbil

A lightweight Gerbil Scheme framework for building native macOS desktop apps with WebKit (WKWebView). Ported from [webkit-cl](https://github.com/markwatson/loving-common-lisp/tree/main/src/webkit-cl) (Common Lisp).

## Overview

webkit-gerbil lets you build small GUI applications in Gerbil Scheme using HTML/CSS/JavaScript for the UI, rendered in the system WebKit engine (WKWebView on macOS). Your Scheme code controls the native window and communicates with the web UI through a bidirectional bridge.

**What it implements:**
- Native macOS windows with embedded WKWebView
- Load HTML content inline, from files, or from URLs
- JavaScript ↔ Gerbil Scheme bridge (`window.webkit_cl.invoke()`)
- Window lifecycle management (create, resize, title, close)
- Multiple named bridge command handlers
- JSON-based message passing

**macOS only** — no Linux, Windows, iOS, or Android support.

## Architecture

```
+-------------------------------------+
|       Gerbil Scheme App             |
|  (compiled via gxc)                |
+-------------------------------------+
|     webkit-gerbil High-Level API    |
|  (app, bridge, window management)   |
+-------------------------------------+
|        Gambit FFI Bindings          |
|  (ffi.ss via :std/foreign)         |
+-------------------------------------+
|      C Shim (webkit_cl.m)          |
|  (Objective-C -> C API bridge)      |
+-------------------------------------+
|    macOS Cocoa + WebKit Framework   |
|  (NSApplication, WKWebView, etc.)   |
+-------------------------------------+
```

## Prerequisites

- macOS (Apple Silicon or Intel)
- Gerbil Scheme (with Gambit)
- Xcode command-line tools (for `clang`, Cocoa, WebKit frameworks)

## Quick Start

### 1. Build everything

```bash
make
```

This compiles `webkit_cl.m` into `libwebkit_cl.dylib` and builds all three example executables.

**Note:** Because the FFI uses `c-declare`/`c-define`, these apps cannot be run interactively with `gxi`. They must be compiled as standalone executables via `gxc`.

### 2. Run the examples

```bash
./hello-world         # Minimal inline HTML app
./counter-app         # Interactive counter with JS <-> Scheme bridge
./markdown-viewer     # File browser via bridge
```

### 3. Build individual examples

```bash
make hello     # just the hello-world example
make counter   # just the counter app
make viewer    # just the markdown viewer
```

## API Overview

### Creating an App

```scheme
(import "webkit-gerbil")
(export main)

(def (main . args)
  (let ((a (create-app title: "My App" width: 800 height: 600)))
    ;; Load inline HTML
    (load-html "<h1>Hello from Gerbil!</h1>" a)
    (app-run a)
    (app-destroy a)))
```

### Bridge: JS -> Scheme

Register handlers that JavaScript can call:

```scheme
(register-handler "greet"
  (lambda (payload)
    (json-response "message"
      (string-append "Hello, " (hash-ref payload 'name) "!"))))
```

From JavaScript:
```javascript
const result = await window.webkit_cl.invoke("greet", { name: "World" });
console.log(result.message); // "Hello, World!"
```

### Bridge: Scheme -> JS

Evaluate JavaScript from Scheme:

```scheme
(eval-js "document.title = 'Updated from Gerbil'")
```

## Project Structure

```
webkit-gerbil/
+-- Makefile                  # Build dylib + compile examples
+-- README.md
+-- webkit_cl.m               # Objective-C shim (Cocoa + WKWebView -> C API)
+-- webkit_cl.h               # C API header
+-- ffi.ss                    # Gambit FFI bindings (c-lambda, c-define)
+-- webkit-gerbil.ss          # High-level Gerbil API
+-- examples/
    +-- hello-world.ss        # Minimal inline HTML example
    +-- counter-app.ss        # Interactive counter with bridge
    +-- markdown-viewer.ss    # Local file viewer via bridge
```

## License

Apache-2.0
