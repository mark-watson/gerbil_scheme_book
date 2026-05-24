;;; -*- Gerbil -*-
;;; daily_use/cache.ss — File-based persistent cache for Gemini answers
;;;
;;; Stores entries as s-expression pairs in a file, one per line:
;;;   (timestamp . "answer text")
;;;
;;; Provides add, lookup (bag-of-words matching), count, clear, and
;;; clear-older-than-one-week operations.

(import :std/sugar :std/error)
(export #t)

(def (make-cache path)
  (let* ((entries (load-cache-entries path))
         (ht (make-hash-table)))
    (hash-put! ht 'path path)
    (hash-put! ht 'entries entries)
    ht))

(def (load-cache-entries path)
  (if (file-exists? path)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (call-with-input-file path
          (lambda (port)
            (let lp ((items '()))
              (let ((item (read port)))
                (if (eof-object? item)
                  (reverse items)
                  (lp (cons item items)))))))))
    '()))

(def (save-cache-entries cache)
  (call-with-output-file (hash-ref cache 'path)
    (lambda (port)
      (for-each (lambda (entry)
                  (write entry port)
                  (newline port))
                (hash-ref cache 'entries)))))

(def (cache-add cache text)
  (let ((entry (cons (time->seconds (current-time)) text)))
    (hash-put! cache 'entries
               (append (hash-ref cache 'entries) (list entry)))
    (save-cache-entries cache)))

(def (cache-count cache)
  (length (hash-ref cache 'entries)))

(def (cache-lookup cache keywords (limit 10))
  (let* ((entries (hash-ref cache 'entries))
         (matching
           (filter (lambda (entry)
                     (any (lambda (kw)
                            (string-contains-ci? (cdr entry) kw))
                          keywords))
                   entries)))
    (map cdr (take matching limit))))

(def (cache-clear cache)
  (hash-put! cache 'entries '())
  (save-cache-entries cache))

(def (cache-clear-older-one-week cache)
  (let* ((one-week-ago (- (time->seconds (current-time)) (* 7 24 60 60)))
         (entries (hash-ref cache 'entries))
         (kept (filter (lambda (entry)
                         (>= (car entry) one-week-ago))
                       entries)))
    (hash-put! cache 'entries kept)
    (save-cache-entries cache)))

(def (cache-close cache)
  (save-cache-entries cache))

(def (take lst n)
  (let lp ((lst lst) (n n) (acc '()))
    (if (or (null? lst) (<= n 0))
      (reverse acc)
      (lp (cdr lst) (- n 1) (cons (car lst) acc)))))

(def (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(def (string-contains-ci? haystack needle)
  (let ((hl (string-length haystack))
        (nl (string-length needle)))
    (let lp ((i 0))
      (if (> (+ i nl) hl)
        #f
        (if (string-ci=? (substring haystack i (+ i nl)) needle)
          #t
          (lp (+ i 1)))))))
