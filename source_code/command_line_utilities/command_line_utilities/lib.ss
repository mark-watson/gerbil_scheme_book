;;; -*- Gerbil -*-
(import :std/error
        :std/sugar)
(export #t)

;;; Your library support code
;;; ...

(def (copy-file-to-stdout path)
  (call-with-input-file path
    (lambda (in)
      (let loop ((b (read-u8 in)))
        (unless (eof-object? b)
          (write-u8 b)
          (loop (read-u8 in)))))))
