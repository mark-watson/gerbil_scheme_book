;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;;
;;; approval.ss -- colored unified diffs and the questions put to the user.

(import :std/sugar
        :std/format
        :std/srfi/13
        "config"
        "util")

(export unified-diff
        print-colored-diff
        ask-choice
        ask-reason)

;; unified-diff : string string string string -> string   (runs `diff -u`)
(def (unified-diff old-content new-content old-label new-label)
  (let ((old-path (temp-file-name "gxh-old"))
        (new-path (temp-file-name "gxh-new")))
    (try
     (write-file old-path old-content)
     (write-file new-path new-content)
     (let-values (((out code) (run-command ["diff" "-u" "-L" old-label "-L" new-label old-path new-path])))
       out)
     (finally
      (when (file-exists? old-path) (delete-file old-path))
      (when (file-exists? new-path) (delete-file new-path))))))

(def (print-colored-diff diff-text)
  (for-each
   (lambda (line)
     (displayln
      (cond
       ((or (string-prefix? "+++" line) (string-prefix? "---" line) (string-prefix? "@@" line))
        (colorize CYAN line))
       ((string-prefix? "+" line) (colorize GREEN line))
       ((string-prefix? "-" line) (colorize RED line))
       (else line))))
   (string-split diff-text #\newline)))

;; ask-choice : string (listof (cons string symbol)) symbol -> symbol
;; `choices` maps accepted answers to results, e.g. '(("y" . yes) ("n" . no)).
;; Returns `on-eof` when stdin is closed or the session is not interactive,
;; so a piped run never hangs on a question nobody can answer.
(def (ask-choice question choices on-eof)
  (if (not (interactive?))
    on-eof
    (let loop ()
      (let ((line (prompt-line question)))
        (cond
         ((eof-object? line) on-eof)
         ((assoc (string-downcase line) choices) => cdr)
         (else
          (displayln (format "Please answer one of: ~a"
                             (string-join (map (lambda (c) (if (string=? (car c) "") "[Enter]" (car c)))
                                               choices)
                                          ", ")))
          (loop)))))))

(def (ask-reason)
  (let ((line (prompt-line "Reason (one line): ")))
    (if (eof-object? line) "" line)))
