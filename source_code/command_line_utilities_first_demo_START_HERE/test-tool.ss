;; test-mytool.ss
(export main)
(import :std/cli/getopt :std/cli/print-exit)
(import :std/format) ;; for 'format'

(def (usage)
  (print-exit 2 "usage: mytool [-v] --name=STR [ARGS...]"))

(def (main . argv)
  (let* ((parser (getopt (flag 'verbose "-v" "--verbose")
                         (option 'name "--name" help: "Your name" value: identity)))
         (opts (getopt-parse parser argv))
         (name (hash-get opts 'name))
         (verbose (hash-get opts 'verbose)))
    (unless name (usage))
    (when verbose (display "verbose on\n"))
    (displayln (format "Hello, ~a\n" name))
    0))


