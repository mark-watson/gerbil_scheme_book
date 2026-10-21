;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; util.ss -- subprocesses, ANSI colors, small string and file helpers.

(import :std/sugar
        :std/format
        :std/srfi/13
        :std/misc/ports
        :std/os/temporaries
        "config")

(export run-command
        run-command/timeout
        colorize
        RED GREEN CYAN YELLOW BOLD
        truncate-string
        trim
        split-words
        file-directory?
        ensure-parent-directory
        write-file
        read-file
        temp-file-name
        prompt-line)

;; ---------------------------------------------------------------------------
;; Subprocesses

;; run-command : (listof string) -> (values output exit-code)
;; stdout and stderr are merged into one string, the way a person would see
;; them in a terminal. No shell is involved, so arguments need no quoting.
(def (run-command args directory: (directory #f))
  (try
   (let ((p (open-process [path: (car args) arguments: (cdr args)
                           directory: (or directory (current-directory))
                           stdin-redirection: #t stdout-redirection: #t
                           stderr-redirection: #t])))
     (close-output-port p)
     (let* ((out (read-all-as-string p))
            (status (process-status p)))
       (close-port p)
       (values out (quotient status 256))))
   (catch (e)
     (values (format "could not run ~a: ~a" (car args) (error-message-of e)) 127))))

;; Same, but kill the process after `seconds` of silence-to-completion.
;; Returns (values output exit-code timed-out?).
(def (run-command/timeout args seconds)
  (let ((p (open-process [path: (car args) arguments: (cdr args)
                          stdin-redirection: #t stdout-redirection: #t
                          stderr-redirection: #t])))
    (close-output-port p)
    (input-port-timeout-set! p seconds (lambda () #f))
    (let* ((out (read-all-as-string p))
           (status (process-status p 0.5 #f)))
      (if status
        (begin (close-port p) (values out (quotient status 256) #f))
        (begin
          (run-command ["kill" "-9" (number->string (process-pid p))])
          (close-port p)
          (values out 124 #t))))))

(def (error-message-of e)
  (if (error-object? e) (error-object-message e) (call-with-output-string (lambda (p) (display e p)))))

;; ---------------------------------------------------------------------------
;; ANSI colors (disabled by --plain / --no-color)

(def RED "\033[31m")
(def GREEN "\033[32m")
(def YELLOW "\033[33m")
(def CYAN "\033[36m")
(def BOLD "\033[1m")
(def RESET "\033[0m")

(def (colorize color text)
  (if (color?) (string-append color text RESET) text))

;; ---------------------------------------------------------------------------
;; Strings

(def (trim s) (string-trim-both s))

(def (split-words s)
  (filter (lambda (w) (not (string=? w "")))
          (string-split (string-map (lambda (c) (if (char=? c #\tab) #\space c)) s) #\space)))

(def (truncate-string s max-len)
  (if (> (string-length s) max-len)
    (string-append (substring s 0 max-len)
                   (format "\n... (truncated, ~a total chars)" (string-length s)))
    s))

;; Read one trimmed line from the user after printing `prompt`; #!eof at EOF.
(def (prompt-line prompt)
  (display prompt)
  (force-output)
  (let ((line (read-line)))
    (if (eof-object? line) #!eof (trim line))))

;; ---------------------------------------------------------------------------
;; Files

(def (file-directory? path)
  (and (file-exists? path)
       (eq? (file-info-type (file-info path)) 'directory)))

(def (ensure-parent-directory path)
  (let ((dir (path-directory (path-expand path))))
    (unless (file-exists? dir)
      (create-directory* dir))))

(def (create-directory* dir)
  (let ((parent (path-directory (string-trim-right dir #\/))))
    (unless (or (string=? parent "") (file-exists? parent))
      (create-directory* parent))
    (unless (file-exists? dir)
      (create-directory dir))))

(def (write-file path content)
  (ensure-parent-directory path)
  (call-with-output-file [path: path truncate: #t create: 'maybe]
    (lambda (out) (display content out))))

(def (read-file path) (read-file-string path))

(def (temp-file-name tag)
  (make-temporary-file-name tag))
