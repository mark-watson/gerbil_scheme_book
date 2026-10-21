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
;;; On top of the shared "providers" section this harness reads an optional
;;; "gerbil" section:
;;;
;;;   "gerbil": {
;;;     "skill":          "gerbil-expert",   ; ~/.agents/skills/<skill>/SKILL.md
;;;     "autoload_skill": true,              ; put it in the system prompt
;;;     "examples_dir":   "~/GITHUB/gerbil_scheme_book/source_code",
;;;     "check_command":  "make check",
;;;     "ask_user_on":    "syntax",          ; "syntax" | "reader"
;;;     "max_iterations": 30,
;;;     "eval_timeout":   20
;;;   }

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
        gerbil-setting
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
;; The "gerbil" section, with the harness defaults

(def gerbil-defaults
  (hash (skill "gerbil-expert")
        (autoload_skill #t)
        (examples_dir "~/GITHUB/gerbil_scheme_book/source_code")
        (check_command "make check")
        (ask_user_on "syntax")
        (max_iterations 30)
        (eval_timeout 20)))

(def (gerbil-setting key)
  (let ((section (json-ref (harness-config) 'gerbil)))
    (if (and (hash-table? section) (hash-key? section key) (not (void? (hash-ref section key))))
      (hash-ref section key)
      (hash-get gerbil-defaults key))))
