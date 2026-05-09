;;; webkit-gerbil.ss — High-level API for webkit-gerbil
;;;
;;; Provides the user-facing functions for creating and managing
;;; WebKit-based GUI applications from Gerbil Scheme.
;;;
;;; Port of webkit-cl (Common Lisp) to Gerbil Scheme.

(import :std/text/json
        :std/format
        "ffi")

(export create-app app-run app-quit app-destroy
        load-html load-url load-file
        eval-js set-title set-size set-resizable
        register-handler unregister-handler
        json-response)

;;; ── Bridge Handler Registry ────────────────────────────────────

(def *bridge-handlers* (make-hash-table))

(def (register-handler command handler-fn)
  "Register a bridge handler for COMMAND.
   HANDLER-FN takes one argument: a parsed JSON payload (hash-table).
   It should return a JSON string to send back to JavaScript, or #f."
  (hash-put! *bridge-handlers* command handler-fn)
  command)

(def (unregister-handler command)
  "Remove the bridge handler for COMMAND."
  (hash-remove! *bridge-handlers* command)
  command)

(def (escape-json-string str)
  "Escape special characters for JSON string values."
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (when (< i (string-length str))
        (let ((ch (string-ref str i)))
          (cond
            ((char=? ch #\") (display "\\\"" out))
            ((char=? ch #\\) (display "\\\\" out))
            ((char=? ch #\newline) (display "\\n" out))
            ((char=? ch #\return) (display "\\r" out))
            ((char=? ch #\tab) (display "\\t" out))
            (else (write-char ch out))))
        (loop (+ i 1))))
    (get-output-string out)))

(def (dispatch-bridge-command command payload-json)
  "Dispatch a bridge command to the registered handler.
   Returns a JSON string, or #f."
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

;;; ── App Structure ──────────────────────────────────────────────

;; Use a simple vector as the app record: #(handle title width height)
(def (%make-app handle title width height)
  (vector handle title width height))

(def (app-handle a) (vector-ref a 0))
(def (app-title a)  (vector-ref a 1))
(def (app-width a)  (vector-ref a 2))
(def (app-height a) (vector-ref a 3))

;;; ── App Lifecycle ──────────────────────────────────────────────

(def *current-app* #f)

(def (create-app title: (title "webkit-gerbil")
                 width: (width 800)
                 height: (height 600))
  "Create a new webkit-gerbil application (does not start the event loop).
   Returns an app record. Call app-run to show the window."
  (let* ((handle (wkcl-create title width height))
         (a (%make-app handle title width height)))
    ;; Install the bridge dispatcher
    (set-bridge-dispatcher! dispatch-bridge-command)
    (wkcl-install-bridge handle)
    a))

(def (app-run a)
  "Start the application event loop (blocks until the window is closed)."
  (set! *current-app* a)
  (wkcl-run (app-handle a))
  (set! *current-app* #f))

(def (app-quit (a *current-app*))
  "Request the application to quit."
  (when a
    (wkcl-quit (app-handle a))))

(def (app-destroy a)
  "Destroy the application and free native resources."
  (when a
    (wkcl-destroy (app-handle a))))

;;; ── Content Loading ────────────────────────────────────────────

(def (load-html html (a *current-app*))
  "Load inline HTML content into the WebView."
  (when a
    (wkcl-load-html (app-handle a) html)))

(def (load-url url (a *current-app*))
  "Navigate the WebView to a URL."
  (when a
    (wkcl-load-url (app-handle a) url)))

(def (load-file path (a *current-app*))
  "Load a local HTML file into the WebView."
  (when a
    (wkcl-load-file (app-handle a) path)))

;;; ── JavaScript ─────────────────────────────────────────────────

(def (eval-js js (a *current-app*))
  "Evaluate JavaScript in the WebView (fire-and-forget)."
  (when a
    (wkcl-eval-js (app-handle a) js)))

;;; ── Window Management ──────────────────────────────────────────

(def (set-title title (a *current-app*))
  "Change the window title."
  (when a
    (wkcl-set-title (app-handle a) title)))

(def (set-size width height (a *current-app*))
  "Resize the window."
  (when a
    (wkcl-set-size (app-handle a) width height)))

(def (set-resizable resizable? (a *current-app*))
  "Set whether the window is resizable."
  (when a
    (wkcl-set-resizable (app-handle a) (if resizable? 1 0))))

;;; ── Convenience ────────────────────────────────────────────────

(def (json-response . pairs)
  "Build a JSON object string from key/value pairs.
   Example: (json-response \"message\" \"hello\" \"count\" 42)
   => {\"message\":\"hello\",\"count\":42}"
  (let ((ht (make-hash-table)))
    (let loop ((p pairs))
      (when (and (pair? p) (pair? (cdr p)))
        (hash-put! ht (car p) (cadr p))
        (loop (cddr p))))
    (json-object->string ht)))
