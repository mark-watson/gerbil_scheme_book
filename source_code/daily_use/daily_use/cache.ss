;;; -*- Gerbil -*-
;;; daily_use/cache.ss — File-based persistent cache for Gemini answers
;;;
;;; Stores entries as s-expression pairs in a file, one per line:
;;;   (timestamp . "answer text")
;;;
;;; Provides add, lookup (keyword-ranked matching), count, clear, and
;;; clear-older-than-one-week operations.

(import :std/sugar
        :std/error
        :std/sort
        :std/srfi/1)

(export make-cache
        load-cache-entries
        save-cache-entries
        cache-add
        cache-count
        cache-lookup
        cache-clear
        cache-clear-older-one-week
        cache-close)

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

(def (string-contains-ci? haystack needle)
  (let ((hl (string-length haystack))
        (nl (string-length needle)))
    (let lp ((i 0))
      (if (> (+ i nl) hl)
        #f
        (if (string-ci=? (substring haystack i (+ i nl)) needle)
          #t
          (lp (+ i 1)))))))

(def (count-matching-keywords text keywords)
  (let loop ((kws keywords) (cnt 0))
    (if (null? kws)
      cnt
      (loop (cdr kws)
            (if (string-contains-ci? text (car kws))
              (+ cnt 1)
              cnt)))))

(def (cache-lookup cache keywords (limit 10))
  (let* ((entries (hash-ref cache 'entries))
         (scored
          (filter-map (lambda (entry)
                        (let ((score (count-matching-keywords (cdr entry) keywords)))
                          (if (> score 0)
                            (cons score (cdr entry))
                            #f)))
                      entries))
         (sorted
          (sort scored (lambda (a b) (> (car a) (car b))))))
    (map cdr (take (if (< (length sorted) limit) sorted (take sorted limit))
                   (min limit (length sorted))))))

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
