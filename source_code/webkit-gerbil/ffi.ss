;;; ffi.ss — Gerbil FFI bindings to libwebkit_cl.dylib
;;;
;;; Low-level foreign function definitions matching webkit_cl.h.
;;; Uses Gambit's c-lambda / c-define via Gerbil's :std/foreign.
;;; All strings use UTF-8-string for proper Unicode support.

(import :std/foreign)

(export wkcl-create wkcl-run wkcl-quit wkcl-destroy
        wkcl-load-html wkcl-load-url wkcl-load-file
        wkcl-eval-js
        wkcl-set-title wkcl-set-size wkcl-set-resizable
        wkcl-install-bridge
        set-bridge-dispatcher!)

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

  ;; The dispatcher variable — defined INSIDE begin-ffi so c-define can see it
  (define *bridge-dispatcher* #f)

  (define (set-bridge-dispatcher! proc)
    (set! *bridge-dispatcher* proc))

  ;; ── Scheme → C bridge function ────────────────────────────────
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
    const char* result = scheme_bridge_cb((char*)command, (char*)payload);
    if (result && result[0] != '\0') {
        return strdup(result);
    }
    return NULL;
}
'C2'
  )

  ;; ── Lifecycle ─────────────────────────────────────────────────

  (define-c-lambda wkcl-create
    (UTF-8-string int int) (pointer void)
    "wkcl_create")

  (define-c-lambda wkcl-run
    ((pointer void)) void
    "wkcl_run")

  (define-c-lambda wkcl-quit
    ((pointer void)) void
    "wkcl_quit")

  (define-c-lambda wkcl-destroy
    ((pointer void)) void
    "wkcl_destroy")

  ;; ── Content Loading ───────────────────────────────────────────

  (define-c-lambda wkcl-load-html
    ((pointer void) UTF-8-string) void
    "wkcl_load_html")

  (define-c-lambda wkcl-load-url
    ((pointer void) UTF-8-string) void
    "wkcl_load_url")

  (define-c-lambda wkcl-load-file
    ((pointer void) UTF-8-string) void
    "wkcl_load_file")

  ;; ── JavaScript ────────────────────────────────────────────────

  (define-c-lambda wkcl-eval-js
    ((pointer void) UTF-8-string) void
    "wkcl_eval_js")

  ;; ── Window Management ─────────────────────────────────────────

  (define-c-lambda wkcl-set-title
    ((pointer void) UTF-8-string) void
    "wkcl_set_title")

  (define-c-lambda wkcl-set-size
    ((pointer void) int int) void
    "wkcl_set_size")

  (define-c-lambda wkcl-set-resizable
    ((pointer void) int) void
    "wkcl_set_resizable")

  ;; ── Install Bridge ────────────────────────────────────────────
  (define-c-lambda wkcl-install-bridge
    ((pointer void)) void
    "wkcl_set_bridge_callback(___arg1, c_bridge_trampoline, NULL);")
)
