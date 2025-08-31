;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        :std/misc/process
        ./lib)
(export main)

(def (print-usage)
  (displayln "usage: command_line_utilities [pwd|ls] [--file PATH]"))

;; copy-file-to-stdout moved to lib.ss

(def (cmd-pwd)
  (displayln (current-directory)))

(def (cmd-ls)
  (try
   (let* ((dir (current-directory))
          (files (directory-files dir)))
     (for-each (lambda (f) (displayln f)) files))
   (catch (e)
     ;; Fallback to external ls if directory-files isn't available
     (run-process ["/bin/ls"] stdout-redirection: #f stderr-redirection: #f))))

(def (main . argv)
  ;; Simple manual parsing to support: pwd | ls | --file PATH
  (let parse ((rest argv) (file #f) (cmd #f))
    (cond
     ((null? rest)
      (when file (copy-file-to-stdout file))
      (cond
       ((eq? cmd 'pwd) (cmd-pwd))
       ((eq? cmd 'ls) (cmd-ls))
       ((not file) (print-usage))))
     (else
      (let* ((arg (car rest))
             (more (cdr rest)))
        (cond
         ((string=? arg "--file")
          (if (pair? more)
              (parse (cdr more) (car more) cmd)
              (begin (displayln "--file requires a path") (print-usage))))
         ((string=? arg "pwd") (parse more file 'pwd))
         ((string=? arg "ls") (parse more file 'ls))
         (else (displayln (string-append "Unknown argument: " arg))
               (print-usage))))))))
