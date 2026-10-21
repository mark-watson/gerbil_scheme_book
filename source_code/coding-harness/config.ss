;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; config.ss -- hierarchical JSON configuration plus the session-wide flags.
;;;
;;; Same two layers (and same provider format) as the Racket and Python
;;; harnesses, so one ~/.coding_harness.json serves all of them:
;;;
;;;   Global:  ~/.coding_harness.json
;;;   Local:   ./.local_coding_harness.json   (wins on conflicts)
;;;
;;; Nested objects merge recursively; anything else is replaced wholesale.
;;; On top of the shared "providers" section this harness reads:
;;;
;;;   "language": "python",                ; force the project language
;;;   "harness": {                         ; settings for every language
;;;     "check_command":  "make check",
;;;     "ask_user_on":    "reader",        ; "syntax" | "reader" | "never"
;;;     "max_iterations": 30,
;;;     "eval_timeout":   20
;;;   },
;;;   "languages": {                       ; per-language overrides
;;;     "python": { "check_command": "pytest -q", "skill": "python-dev" },
;;;     "gerbil": { "examples_dir": "~/GITHUB/gerbil_scheme_book/source_code" }
;;;   }
;;;
;;; A setting is looked up in languages.<current language>, then harness,
;;; then the language profile's defaults (lang.ss), then the harness
;;; defaults. The older top-level "gerbil" section still works: it counts as
;;; languages.gerbil.

(import :std/sugar
        :std/format
        :std/sort
        :std/text/json)

(export load-harness-config
        harness-config
        config-provider-names
        config-provider
        config-active-provider-name
        config-set-active-provider!
        config-active-provider
        provider-endpoint
        provider-model
        provider-api-key-env
        provider-generation
        provider-pricing
        json-ref
        setting
        current-language
        language-defaults
        global-config-path
        local-config-path
        ;; session flags (parameters, so every module sees one value)
        auto-approve?
        dry-run?
        quiet?
        color?
        debug?
        interactive?)

(def auto-approve? (make-parameter #f))
(def dry-run?      (make-parameter #f))
(def quiet?        (make-parameter #f))
(def color?        (make-parameter #t))
(def debug?        (make-parameter #f))
;; #f when stdin is not a person at a terminal (piped prompt, CI): then the
;; harness never blocks on a y/n question it cannot get an answer to.
(def interactive?  (make-parameter #t))

;; ---------------------------------------------------------------------------
;; JSON helpers

;; Parsed JSON objects have symbol keys; accept a string or symbol key.
(def (json-ref obj key (default #f))
  (if (hash-table? obj)
    (let* ((k (if (string? key) (string->symbol key) key))
           (v (hash-ref obj k default)))
      (if (void? v) default v))     ; JSON null reads as #!void
    default))

(def (read-json-file path)
  (try
   (and (file-exists? path)
        (let ((v (call-with-input-file path read-json)))
          (and (hash-table? v) v)))
   (catch (e)
     (displayln (format "warning: could not parse ~a; ignoring it" path))
     #f)))

(def (global-config-path) (path-expand "~/.coding_harness.json"))
(def (local-config-path) (path-expand ".local_coding_harness.json" (current-directory)))

(def (deep-merge global local)
  (if (and (hash-table? global) (hash-table? local))
    (let ((acc (hash-copy global)))
      (hash-for-each
       (lambda (k v)
         (let ((g (hash-get acc k)))
           (hash-put! acc k (if (and (hash-table? g) (hash-table? v)) (deep-merge g v) v))))
       local)
      acc)
    local))

(def harness-config (make-parameter (make-hash-table)))

(def (load-harness-config)
  (let ((merged (deep-merge (or (read-json-file (global-config-path)) (make-hash-table))
                            (or (read-json-file (local-config-path)) (make-hash-table)))))
    (harness-config merged)
    merged))

;; ---------------------------------------------------------------------------
;; Providers

(def (config-providers)
  (let ((p (json-ref (harness-config) 'providers)))
    (if (hash-table? p) p (make-hash-table))))

(def (config-provider-names)
  (sort (map symbol->string (hash-keys (config-providers))) string<?))

(def (config-provider name)
  (and name (json-ref (config-providers) name)))

(def active-provider-name (make-parameter #f))

(def (config-active-provider-name)
  (or (active-provider-name)
      (let* ((declared (json-ref (harness-config) 'default_provider))
             (names (config-provider-names))
             (pick (cond
                    ((and (string? declared) (config-provider declared)) declared)
                    ((pair? names) (car names))
                    (else #f))))
        (active-provider-name pick)
        pick)))

(def (config-set-active-provider! name)
  (when (config-provider name)
    (active-provider-name name))
  (active-provider-name))

(def (config-active-provider)
  (config-provider (config-active-provider-name)))

(def (nonempty-string v) (and (string? v) (not (string=? v "")) v))

(def (provider-endpoint p) (nonempty-string (json-ref p 'endpoint)))
(def (provider-model p) (nonempty-string (json-ref p 'model)))
(def (provider-api-key-env p) (nonempty-string (json-ref p 'api_key_env)))

(def (provider-generation p)
  (let ((g (json-ref p 'generation))) (if (hash-table? g) g (make-hash-table))))

(def (provider-pricing p)
  (let ((g (json-ref p 'pricing))) (if (hash-table? g) g (make-hash-table))))

;; ---------------------------------------------------------------------------
;; Settings, layered by language

;; The language profile in use ("python", "gerbil", ...) and its defaults
;; (a hash); lang.ss sets both when a language is selected.
(def current-language (make-parameter "generic"))
(def language-defaults (make-parameter (make-hash-table)))

(def harness-defaults
  (hash (skill #f)
        (autoload_skill #t)
        (examples_dir #f)
        (check_command "make check")
        (ask_user_on "reader")
        (max_iterations 30)
        (eval_timeout 20)))

(def (section-value section key)
  (and (hash-table? section)
       (let ((v (hash-ref section key (void))))
         (and (not (void? v)) (box v)))))

(def (language-section)
  (let* ((cfg (harness-config))
         (langs (json-ref cfg 'languages))
         (own (and (hash-table? langs) (json-ref langs (current-language)))))
    (if (and (string=? (current-language) "gerbil") (hash-table? (json-ref cfg 'gerbil)))
      (if (hash-table? own) (deep-merge (json-ref cfg 'gerbil) own) (json-ref cfg 'gerbil))
      own)))

(def (setting key)
  (let ((found (or (section-value (language-section) key)
                   (section-value (json-ref (harness-config) 'harness) key)
                   (section-value (language-defaults) key))))
    (if found (unbox found) (hash-get harness-defaults key))))
