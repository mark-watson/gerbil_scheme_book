# WebKit Applications - macOS Only

In this chapter we build native macOS desktop applications using Gerbil Scheme and WebKit. The **webkit-gerbil** library lets you create windows with embedded *WKWebView* panels, load HTML/CSS/JavaScript UIs, and communicate between Scheme and JavaScript through a bidirectional bridge. This approach gives you the full power of Gerbil Scheme for application logic while using modern web technologies for the user interface.

This is a port of my Common Lisp [webkit-cl](https://github.com/markwatson/loving-common-lisp/tree/main/src/webkit-cl) library (covered in my book *Loving Common Lisp*). The two projects share the same Objective-C shim and C API, differing only in the FFI bindings and high-level API layer.

**Note 1: This library works only on macOS. It requires Gerbil Scheme (with Gambit) and Xcode command-line tools.**

**Note 2: Because the FFI uses Gambit's c-declare/c-define, these apps must be compiled as standalone executables via gxc. They cannot be run interactively with gxi.**

## Architecture Overview

The webkit-gerbil framework is organized in four layers:

1. **Objective-C shim** (`webkit_cl.m`): Bridges macOS Cocoa and WebKit APIs to a flat C interface
2. **Gambit FFI bindings** (`ffi.ss`): Exposes the C functions to Gerbil Scheme via `:std/foreign`
3. **Bridge dispatch** (built into `webkit-gerbil.ss`): Manages JS-to-Scheme command dispatch and JSON serialization
4. **High-level API** (`webkit-gerbil.ss`): Idiomatic Gerbil functions: `create-app`, `load-html`, `register-handler`, etc.

When JavaScript calls `window.webkit_cl.invoke("command", payload)`, the message travels through *WKWebView's* script message handler into the C shim, through Gambit's FFI into Scheme, where a registered handler processes it and returns a JSON response. The response flows back to JavaScript via a Promise.

## Prerequisites and Building

You need macOS (Apple Silicon or Intel), Gerbil Scheme (with Gambit), and Xcode command-line tools (for `clang` and the Cocoa/WebKit frameworks). Build everything with:

{lang="bash",linenos=off}
~~~~~~~~
cd source_code/webkit-gerbil
make
~~~~~~~~

This compiles the Objective-C shim into `libwebkit_cl.dylib` and builds all three example executables:

{linenos=off}
~~~~~~~~
clang -fobjc-arc -fPIC -O2 -Wall -framework Cocoa -framework WebKit \
  -dynamiclib -install_name @rpath/libwebkit_cl.dylib \
  -o libwebkit_cl.dylib webkit_cl.m
gxc -cc-options "..." -ld-options "..." -exe -o hello-world \
  ffi.ss webkit-gerbil.ss examples/hello-world.ss
~~~~~~~~

## Project Structure

The project contains two core modules and three example applications:

{linenos=off}
~~~~~~~~
webkit-gerbil/
  Makefile                  # Build dylib + compile examples
  webkit_cl.m               # Objective-C shim (Cocoa + WKWebView -> C API)
  webkit_cl.h               # C API header
  ffi.ss                    # Gambit FFI bindings (c-lambda, c-define)
  webkit-gerbil.ss          # High-level Gerbil API
  examples/
    hello-world.ss          # Minimal inline HTML example
    counter-app.ss          # Interactive counter with bridge
    markdown-viewer.ss      # Local file viewer via bridge
~~~~~~~~

Unlike the Common Lisp version which uses ASDF, Gerbil's compilation model is simpler: `gxc` compiles all source files together into a standalone executable. The Makefile handles the build flags for linking against `libwebkit_cl.dylib` and the macOS frameworks.

## The C Shim

The C header (`webkit_cl.h`) defines a minimal interface. All functions take an opaque `wkcl_app_t` handle:

{lang="c",linenos=off}
~~~~~~~~
typedef void* wkcl_app_t;

/* Callback for bridge invocations from JavaScript */
typedef const char* (*wkcl_bridge_callback_t)(const char* command,
                                              const char* payload,
                                              void* userdata);

/* Lifecycle */
wkcl_app_t wkcl_create(const char* title, int width, int height);
void wkcl_run(wkcl_app_t app);
void wkcl_quit(wkcl_app_t app);
void wkcl_destroy(wkcl_app_t app);

/* Content loading */
void wkcl_load_html(wkcl_app_t app, const char* html);
void wkcl_load_url(wkcl_app_t app, const char* url);
void wkcl_load_file(wkcl_app_t app, const char* path);

/* JavaScript & Bridge */
void wkcl_eval_js(wkcl_app_t app, const char* js);
void wkcl_set_bridge_callback(wkcl_app_t app,
                               wkcl_bridge_callback_t callback,
                               void* userdata);

/* Window management */
void wkcl_set_title(wkcl_app_t app, const char* title);
void wkcl_set_size(wkcl_app_t app, int width, int height);
void wkcl_set_resizable(wkcl_app_t app, int resizable);
~~~~~~~~

The Objective-C implementation (`webkit_cl.m`) creates an `NSApplication` with a `WKWebView` inside an `NSWindow`. The bridge works by injecting a JavaScript snippet at document start that defines `window.webkit_cl.invoke()`. This function posts messages to a `WKScriptMessageHandler`, which routes them to the registered C callback. The callback returns a `malloc`'d JSON string that is sent back to JavaScript via `evaluateJavaScript:`.

This C API is identical to the one used by the Common Lisp version. Because it compiles to a standalone dynamic library (`libwebkit_cl.dylib`), the same `.m` and `.h` files can be reused by both language bindings without modification.

## Gambit FFI Bindings

The FFI layer in **ffi.ss** maps the C API to Gerbil Scheme using Gambit's `c-lambda`, `c-define`, and `c-declare` forms via Gerbil's `:std/foreign` module. All string parameters use the `UTF-8-string` FFI type for proper Unicode support:

{lang="scheme",linenos=off}
~~~~~~~~
(import :std/foreign)

(export wkcl-create wkcl-run wkcl-quit wkcl-destroy
        wkcl-load-html wkcl-load-url wkcl-load-file
        wkcl-eval-js
        wkcl-set-title wkcl-set-size wkcl-set-resizable
        wkcl-install-bridge set-bridge-dispatcher!)

(begin-ffi (wkcl-create wkcl-run wkcl-quit wkcl-destroy
            wkcl-load-html wkcl-load-url wkcl-load-file
            wkcl-eval-js
            wkcl-set-title wkcl-set-size wkcl-set-resizable
            wkcl-install-bridge
            set-bridge-dispatcher!)

  (c-declare #<<'C'
#include <stdlib.h>
#include <string.h>
#include "webkit_cl.h"
'C'
  )

  ;; The dispatcher variable — defined INSIDE begin-ffi
  ;; so c-define can see it
  (define *bridge-dispatcher* #f)

  (define (set-bridge-dispatcher! proc)
    (set! *bridge-dispatcher* proc))
~~~~~~~~

The bridge callback is defined with `c-define`, which is Gambit's standard mechanism for creating Scheme functions callable from C. When JavaScript calls `window.webkit_cl.invoke()`, the C shim calls our `scheme_bridge_cb` function, which dispatches to the Scheme-level handler:

{lang="scheme",linenos=off}
~~~~~~~~
  ;; c-define makes a Scheme function callable from C
  (c-define (scheme-bridge-callback command payload)
    (UTF-8-string UTF-8-string) UTF-8-string "scheme_bridge_cb" ""
    (if *bridge-dispatcher*
      (let ((result (*bridge-dispatcher* command payload)))
        (if (string? result) result "null"))
      "null"))

  ;; C wrapper that adapts the wkcl_bridge_callback_t signature
  (c-declare #<<'C2'
static const char* c_bridge_trampoline(const char* command,
                                        const char* payload,
                                        void* userdata) {
    const char* result = scheme_bridge_cb((char*)command,
                                          (char*)payload);
    if (result && result[0] != '\0') {
        return strdup(result);
    }
    return NULL;
}
'C2'
  )
~~~~~~~~

A key architectural detail: the `*bridge-dispatcher*` variable and `set-bridge-dispatcher!` function are defined *inside* the `begin-ffi` block. This is essential because `c-define` body expressions can only resolve identifiers that share the same scope. Defining them outside `begin-ffi` would cause `#!unbound` errors at runtime due to Gerbil's module namespace separation.

The C-to-Scheme data flow requires a C trampoline function (`c_bridge_trampoline`) because the bridge callback signature includes a `void* userdata` parameter that `c-define` cannot directly match. The trampoline calls `scheme_bridge_cb`, then `strdup`s the result so the Objective-C side can `free()` it after use.

The remaining FFI definitions are straightforward `define-c-lambda` wrappers:

{lang="scheme",linenos=off}
~~~~~~~~
  (define-c-lambda wkcl-create
    (UTF-8-string int int) (pointer void)
    "wkcl_create")

  (define-c-lambda wkcl-run
    ((pointer void)) void
    "wkcl_run")

  (define-c-lambda wkcl-load-html
    ((pointer void) UTF-8-string) void
    "wkcl_load_html")

  (define-c-lambda wkcl-eval-js
    ((pointer void) UTF-8-string) void
    "wkcl_eval_js")

  ;; Registers the C trampoline as the bridge callback
  (define-c-lambda wkcl-install-bridge
    ((pointer void)) void
    "wkcl_set_bridge_callback(___arg1,
       c_bridge_trampoline, NULL);")
)
~~~~~~~~

Note the use of `UTF-8-string` rather than `char-string`. The standard Gambit `char-string` type only handles ASCII; `UTF-8-string` properly converts Scheme strings containing Unicode characters (such as em dashes in window titles or special characters in HTML content).

## The Bridge: JS to Scheme Communication

The bridge module in **webkit-gerbil.ss** maintains a hash table of named command handlers:

{lang="scheme",linenos=off}
~~~~~~~~
(def *bridge-handlers* (make-hash-table))

(def (register-handler command handler-fn)
  "Register a bridge handler for COMMAND.
   HANDLER-FN takes one argument: a parsed JSON payload.
   It should return a JSON string to send back to JavaScript."
  (hash-put! *bridge-handlers* command handler-fn)
  command)

(def (unregister-handler command)
  "Remove the bridge handler for COMMAND."
  (hash-remove! *bridge-handlers* command)
  command)
~~~~~~~~

When JavaScript calls `window.webkit_cl.invoke("greet", {name: "World"})`, the dispatch function looks up the handler by command name, parses the JSON payload with Gerbil's `:std/text/json`, calls the handler, and returns the result:

{lang="scheme",linenos=off}
~~~~~~~~
(def (dispatch-bridge-command command payload-json)
  "Dispatch a bridge command to the registered handler."
  (let ((handler (hash-get *bridge-handlers* command)))
    (if handler
      (with-catch
        (lambda (e)
          (format "{\"error\": \"~a\"}"
                  (escape-json-string (format "~a" e))))
        (lambda ()
          (let* ((payload (with-catch
                            (lambda (e) #f)
                            (lambda ()
                              (string->json-object payload-json))))
                 (result (handler payload)))
            (if result result "null"))))
      (format "{\"error\": \"unknown command: ~a\"}"
              (escape-json-string command)))))
~~~~~~~~

The double `with-catch` nesting mirrors the Common Lisp version's `handler-case` pattern: one catches JSON parse errors (which are non-fatal, the handler receives `#f`), the other catches handler execution errors and returns them as JSON error objects to JavaScript.

## High-Level API

The main API provides app lifecycle management and convenience functions. The `create-app` function creates the native window, installs the bridge dispatcher, and returns an app record:

{lang="scheme",linenos=off}
~~~~~~~~
(def *current-app* #f)

(def (create-app title: (title "webkit-gerbil")
                 width: (width 800)
                 height: (height 600))
  "Create a new webkit-gerbil application."
  (let* ((handle (wkcl-create title width height))
         (a (%make-app handle title width height)))
    (set-bridge-dispatcher! dispatch-bridge-command)
    (wkcl-install-bridge handle)
    a))

(def (app-run a)
  "Start the event loop (blocks until the window is closed)."
  (set! *current-app* a)
  (wkcl-run (app-handle a))
  (set! *current-app* #f))

(def (app-destroy a)
  "Destroy the application and free native resources."
  (when a
    (wkcl-destroy (app-handle a))))
~~~~~~~~

Content loading and JavaScript evaluation are thin wrappers around the C API. Each function accepts an optional app argument that defaults to `*current-app*`:

{lang="scheme",linenos=off}
~~~~~~~~
(def (load-html html (a *current-app*))
  "Load inline HTML content into the WebView."
  (when a
    (wkcl-load-html (app-handle a) html)))

(def (load-url url (a *current-app*))
  "Navigate the WebView to a URL."
  (when a
    (wkcl-load-url (app-handle a) url)))

(def (eval-js js (a *current-app*))
  "Evaluate JavaScript in the WebView (fire-and-forget)."
  (when a
    (wkcl-eval-js (app-handle a) js)))
~~~~~~~~

The `json-response` convenience function builds JSON strings from key/value pairs using Gerbil's `:std/text/json`:

{lang="scheme",linenos=off}
~~~~~~~~
(def (json-response . pairs)
  "Build a JSON object string from key/value pairs.
   Example: (json-response \"message\" \"hello\" \"count\" 42)"
  (let ((ht (make-hash-table)))
    (let loop ((p pairs))
      (when (and (pair? p) (pair? (cdr p)))
        (hash-put! ht (car p) (cadr p))
        (loop (cddr p))))
    (json-object->string ht)))
~~~~~~~~

## Example 1: Hello World

The simplest webkit-gerbil app loads inline HTML into a native window. Each example exports a `main` function as required by `gxc -exe`:

{lang="scheme",linenos=off}
~~~~~~~~
(import "../webkit-gerbil")
(export main)

(def (main . args)
  (let ((a (create-app title: "Hello webkit-gerbil"
                       width: 600 height: 400)))
    (load-html
     "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, system-ui, sans-serif;
    background: linear-gradient(135deg,
      #0f0c29 0%, #302b63 50%, #24243e 100%);
    color: #e0e0e0;
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
  }
  .card {
    text-align: center;
    background: rgba(255,255,255,0.05);
    backdrop-filter: blur(20px);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 24px;
    padding: 48px 64px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.3);
  }
  h1 {
    font-size: 2.5em;
    background: linear-gradient(90deg, #a78bfa, #60a5fa, #34d399);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    margin-bottom: 12px;
  }
  p { font-size: 1.1em; color: rgba(255,255,255,0.6); }
  .badge {
    display: inline-block; margin-top: 20px;
    padding: 6px 16px; font-size: 0.85em;
    background: rgba(167,139,250,0.15);
    border: 1px solid rgba(167,139,250,0.3);
    border-radius: 999px; color: #a78bfa;
  }
</style>
</head>
<body>
  <div class='card'>
    <h1>Hello, webkit-gerbil!</h1>
    <p>A native macOS window powered by Gerbil Scheme<br>
       and WebKit (WKWebView).</p>
    <span class='badge'>Gerbil + Gambit + Cocoa + WebKit</span>
  </div>
</body>
</html>"
     a)
    (app-run a)
    (app-destroy a)))
~~~~~~~~

Build and run it with:

{lang="bash",linenos=off}
~~~~~~~~
make hello
./hello-world
~~~~~~~~

A native macOS window appears with a gradient background, glassmorphism card, and gradient text, all rendered by the system WebKit engine.

## Example 2: Counter App with Bridge

This example demonstrates bidirectional communication. Scheme manages the application state (a counter), and JavaScript provides the UI:

{lang="scheme",linenos=off}
~~~~~~~~
(import "../webkit-gerbil")
(export main)

(def *counter* 0)

(register-handler "increment"
  (lambda (payload)
    (set! *counter* (+ *counter* 1))
    (json-response "count" *counter*)))

(register-handler "decrement"
  (lambda (payload)
    (set! *counter* (- *counter* 1))
    (json-response "count" *counter*)))

(register-handler "reset"
  (lambda (payload)
    (set! *counter* 0)
    (json-response "count" *counter*)))

(register-handler "get-count"
  (lambda (payload)
    (json-response "count" *counter*)))
~~~~~~~~

Each handler receives a parsed JSON payload (a hash table from `:std/text/json`) and returns a JSON string. The JavaScript side calls these handlers through the bridge:

{lang="javascript",linenos=off}
~~~~~~~~
async function increment() {
  const result = await window.webkit_cl.invoke('increment', {});
  updateDisplay(result.count);
}

async function decrement() {
  const result = await window.webkit_cl.invoke('decrement', {});
  updateDisplay(result.count);
}
~~~~~~~~

The counter value lives entirely in Scheme -- JavaScript only renders it. This pattern cleanly separates application logic (Scheme) from presentation (HTML/CSS/JS).

Build and run it with:

{lang="bash",linenos=off}
~~~~~~~~
make counter
./counter-app
~~~~~~~~

## Example 3: Markdown File Viewer

The most complete example demonstrates filesystem access through the bridge. Two handlers let JavaScript list and read files:

{lang="scheme",linenos=off}
~~~~~~~~
(register-handler "read-file"
  (lambda (payload)
    (let ((path (and payload (hash-get payload 'path))))
      (if (and path (file-exists? path))
        (let ((content (read-file-string path)))
          (let ((ht (make-hash-table)))
            (hash-put! ht "content" content)
            (hash-put! ht "path" path)
            (json-object->string ht)))
        (json-response "error"
          (string-append "File not found: "
                         (or path "nil")))))))

(register-handler "list-files"
  (lambda (payload)
    (let* ((dir (or (and payload
                         (hash-get payload 'directory)) "."))
           (entries (directory-files dir))
           (md-files (filter
                       (lambda (f)
                         (let ((len (string-length f)))
                           (and (>= len 3)
                                (string=? ".md"
                                  (substring f (- len 3) len)))))
                       entries))
           (full-paths (map (lambda (f)
                              (string-append dir "/" f))
                            md-files)))
      (let ((ht (make-hash-table)))
        (hash-put! ht "files" (list->vector full-paths))
        (json-object->string ht)))))
~~~~~~~~

The `read-file` handler uses Gerbil's `read-file-string` from `:std/misc/ports` to load file contents, then builds a JSON response manually using a hash table to ensure proper escaping. The `list-files` handler uses `directory-files` and filters for `.md` extensions.

The UI is a split-pane layout with a file sidebar and content area. JavaScript calls the bridge on startup:

{lang="javascript",linenos=off}
~~~~~~~~
async function loadFileList() {
  const result = await window.webkit_cl.invoke('list-files',
                                                { directory: '.' });
  if (result.files && result.files.length > 0) {
    fileList.innerHTML = result.files.map(f =>
      '<div class="file-item" onclick="loadFile(\'' + f + '\')">' +
      '<div class="name">' + basename(f) + '</div>' +
      '</div>'
    ).join('');
  }
}

async function loadFile(path) {
  const result = await window.webkit_cl.invoke('read-file',
                                                { path: path });
  if (result.content) {
    content.innerHTML = '<pre>' + escapeHtml(result.content)
                      + '</pre>';
  }
}
~~~~~~~~

Build and run it with:

{lang="bash",linenos=off}
~~~~~~~~
make viewer
./markdown-viewer
~~~~~~~~

This opens a native window with a dark sidebar listing `.md` files from the current directory. Clicking a file reads its content via the Scheme bridge and displays it in a styled code panel.

## The Build System

The Makefile compiles the Objective-C shim and links the Gerbil source files into standalone executables:

{lang="makefile",linenos=off}
~~~~~~~~
CC = clang
CFLAGS = -fobjc-arc -fPIC -O2 -Wall
FRAMEWORKS = -framework Cocoa -framework WebKit
DYNLIB_LDFLAGS = -dynamiclib \
  -install_name @rpath/libwebkit_cl.dylib

GERBIL_CC_OPTS = -I$(CURDIR)
GERBIL_LD_OPTS = -L$(CURDIR) -lwebkit_cl \
  -Wl,-rpath,@loader_path \
  -framework Cocoa -framework WebKit

$(DYLIB): $(SRC_M) webkit_cl.h
	$(CC) $(CFLAGS) $(FRAMEWORKS) $(DYNLIB_LDFLAGS) -o $@ $<

hello: $(DYLIB)
	gxc -cc-options "$(GERBIL_CC_OPTS)" \
	    -ld-options "$(GERBIL_LD_OPTS)" \
	    -exe -o hello-world \
	    ffi.ss webkit-gerbil.ss examples/hello-world.ss
~~~~~~~~

Each example target passes all three `.ss` files to `gxc` together. The `-exe` flag produces a standalone executable that statically links the Gambit runtime. The `-cc-options` flag provides the include path for `webkit_cl.h`, and `-ld-options` links against `libwebkit_cl.dylib` with an `@loader_path` rpath so the executable can find the dylib at runtime.

## Comparing Common Lisp and Gerbil Versions

The Common Lisp and Gerbil versions share the same architecture and C shim but differ in FFI mechanics:

| Aspect | Common Lisp (webkit-cl) | Gerbil (webkit-gerbil) |
|--------|------------------------|----------------------|
| FFI system | CFFI | Gambit `begin-ffi` / `:std/foreign` |
| Callback | `cffi:defcallback` | `c-define` (inside `begin-ffi`) |
| String type | `:string` (CFFI) | `UTF-8-string` (Gambit) |
| JSON | `cl-json` | `:std/text/json` |
| Build | ASDF + `make` (dylib only) | `gxc -exe` + `make` (full executable) |
| Execution | `sbcl --load` (interactive) | Standalone executable only |
| App lifecycle | `with-app` macro | `create-app` / `app-run` / `app-destroy` |

The most significant difference is in callback scoping: CFFI's `defcallback` naturally lives in the package namespace, while Gambit's `c-define` requires the dispatcher variable to be defined inside the same `begin-ffi` block to avoid cross-module resolution issues.

## API Reference Summary

The webkit-gerbil public API:

| Function | Description |
|---|---|
| `(create-app title: width: height:)` | Create app with native window |
| `(app-run app)` | Enter event loop (blocks until window close) |
| `(app-quit app)` | Request the application to quit |
| `(app-destroy app)` | Free native resources |
| `(load-html html)` | Load inline HTML string |
| `(load-url url)` | Navigate to a URL |
| `(load-file path)` | Load a local HTML file |
| `(eval-js js-string)` | Evaluate JavaScript (fire-and-forget) |
| `(register-handler name fn)` | Register a bridge command handler |
| `(unregister-handler name)` | Remove a bridge command handler |
| `(json-response key val ...)` | Build JSON string from key/value pairs |
| `(set-title title)` | Change window title |
| `(set-size width height)` | Resize window |
| `(set-resizable flag)` | Toggle window resizability |

From JavaScript, call Scheme handlers with:

{lang="javascript",linenos=off}
~~~~~~~~
const result = await window.webkit_cl.invoke("command-name",
                                              {key: "value"});
~~~~~~~~

## Key Takeaways

1. **Gambit FFI + Objective-C** -- Gerbil Scheme can drive native macOS frameworks through a thin C shim compiled as a dynamic library, using `c-lambda` and `c-define` from `:std/foreign`
2. **WKWebView** -- The system WebKit engine provides a modern, full-featured rendering surface without bundling a browser
3. **Bridge pattern** -- Named command handlers with JSON message passing cleanly separate Scheme logic from JavaScript UI
4. **`c-define` scoping** -- Bridge callback variables must be defined inside `begin-ffi` to be visible to `c-define` bodies; this is the key Gerbil-specific lesson from this project
5. **`UTF-8-string`** -- Always use `UTF-8-string` instead of `char-string` in FFI bindings when Unicode text is possible
6. **Code reuse** -- The same C/Objective-C shim serves both Common Lisp and Gerbil Scheme, demonstrating the value of a clean C API as a language-agnostic boundary

This framework demonstrates that Gerbil Scheme can build polished desktop applications. The web rendering layer handles the visual complexity while Scheme provides the computational backbone -- a productive division of labor for tools, dashboards, and data viewers.

## Optional Practice Problems

1. **Interactive Click Counter**: Create a small application where clicking a button in HTML sends a JSON message to increment a counter variable in Scheme, and then updates the DOM with the updated value.
2. **Error Logging Bridge**: Implement a bridge handler that allows the web view's JavaScript execution context to log stack traces and uncaught exceptions directly to the Gerbil Scheme stdout or a log file.
3. **Dynamic Window Controls**: Add FFI functions and Scheme wrappers in `webkit-gerbil.ss` to dynamically resize the window or change its title from Scheme code after the app has started running.

