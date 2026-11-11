;; File: webscrape.ss
(import :std/net/request
        :std/format
        :std/pregexp
        :std/srfi/13)

(export get-plain-text
        get-markdown
        get-links
        get-headers
        html->text
        html->markdown
        html->links
        html->headers
        unescape-html-entities
        resolve-url)

;; Default User-Agent: identify our tool honestly per web standards
(def default-user-agent
  (or (getenv "WEBSCRAPE_USER_AGENT" #f)
      "GerbilWebScraper/1.0 (+https://github.com/mark-watson/gerbil_scheme_book; contact@markwatson.com)"))

;; Fetch raw HTML text from a URI
(def (fetch-html uri (user-agent default-user-agent))
  (let* ((headers `(("User-Agent" . ,user-agent)
                    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")))
         (response (http-get uri headers: headers)))
    (if (= (request-status response) 200)
      (request-text response)
      (error "Failed to fetch URI" status: (request-status response) uri: uri))))

;; Find all regex matches in a string and return positional info
(def (find-all-matches pattern str)
  (let loop ((start 0) (acc '()))
    (let ((pos (pregexp-match-positions pattern str start)))
      (if pos
        (let* ((match-end (cdr (car pos)))
               (next-start (if (= match-end start) (+ start 1) match-end)))
          (loop next-start (cons pos acc)))
        (reverse acc)))))

(def (replace-numeric-entities str hex?)
  (let ((pat (if hex? "&#x([0-9a-fA-F]+);" "&#(\\d+);"))
        (base (if hex? 16 10)))
    (let loop ((s str))
      (let ((m (pregexp-match-positions pat s)))
        (if (not m)
            s
            (let* ((full (car m))
                   (grp (cadr m))
                   (digits (substring s (car grp) (cdr grp)))
                   (n (string->number digits base))
                   (ch-str (if (and n (< n 65536))
                               (string (integer->char n))
                               ""))
                   (prefix (substring s 0 (car full)))
                   (suffix (substring s (cdr full) (string-length s))))
              (loop (string-append prefix ch-str suffix))))))))

;; Unescape standard and numeric HTML entities.
;; &amp; is decoded LAST so entities like &amp;lt; decode to &lt;, not <.
(def (unescape-html-entities str)
  (let* ((s (pregexp-replace* "&nbsp;" str " "))
         (s (pregexp-replace* "&lt;" s "<"))
         (s (pregexp-replace* "&gt;" s ">"))
         (s (pregexp-replace* "&quot;" s "\""))
         (s (pregexp-replace* "&#39;" s "'"))
         (s (pregexp-replace* "&apos;" s "'"))
         (s (replace-numeric-entities s #f))
         (s (replace-numeric-entities s #t))
         ;; &amp; decoded last
         (s (pregexp-replace* "&amp;" s "&")))
    s))

;; Clean up consecutive whitespace and newlines
(def (clean-whitespace str)
  (let* ((s (pregexp-replace* "\r" str ""))
         ;; Remove trailing spaces/tabs on each line
         (s (pregexp-replace* "[ \t]+\n" s "\n"))
         ;; Remove leading spaces/tabs on each line
         (s (pregexp-replace* "\n[ \t]+" s "\n"))
         ;; Merge 3 or more consecutive newlines into 2
         (s (pregexp-replace* "\n{3,}" s "\n\n"))
         ;; Replace consecutive spaces/tabs with a single space
         (s (pregexp-replace* "[ \t]+" s " "))
         ;; Trim the entire string
         (s (string-trim s)))
    s))

;; Resolve relative URLs against a base URI
(def (resolve-url base rel)
  (cond
   ((or (not base) (string-null? base)) rel)
   ((or (string-prefix? "http://" rel) (string-prefix? "https://" rel)) rel)
   ((string-prefix? "//" rel) (string-append "https:" rel))
   ((string-prefix? "/" rel)
    (let ((m (pregexp-match "^(https?://[^/]+)" base)))
      (if m (string-append (cadr m) rel) rel)))
   (else
    (let ((m (pregexp-match "^(https?://.*/)" base)))
      (if m (string-append (cadr m) rel) (string-append base "/" rel))))))

;;; ---- Pure Parsers (offline testable) ---------------------------------------

;; Parse plain text from HTML string
(def (html->text html)
  (let* (;; Strip script tags and content
         (h (pregexp-replace* "(?i:<script[^>]*>[\\s\\S]*?</script>)" html ""))
         ;; Strip style tags and content
         (h (pregexp-replace* "(?i:<style[^>]*>[\\s\\S]*?</style>)" h ""))
         ;; Strip HTML comments
         (h (pregexp-replace* "<!--[\\s\\S]*?-->" h ""))
         ;; Replace block elements (including tags with attributes) with newlines
         (h (pregexp-replace* "(?i:</?(?:p|div|tr|table|ul|ol|li|h[1-6])\\b[^>]*>|</?br\\b\\s*/?>)" h "\n"))
         ;; Strip all remaining HTML tags
         (h (pregexp-replace* "<[^>]*>" h " "))
         ;; Resolve HTML entities (&amp; last)
         (h (unescape-html-entities h))
         ;; Standardize whitespace
         (h (clean-whitespace h)))
    h))

;; Parse Markdown from HTML string
(def (html->markdown html)
  (let* (;; Strip script, style, comments
         (h (pregexp-replace* "(?i:<script[^>]*>[\\s\\S]*?</script>)" html ""))
         (h (pregexp-replace* "(?i:<style[^>]*>[\\s\\S]*?</style>)" h ""))
         (h (pregexp-replace* "<!--[\\s\\S]*?-->" h ""))
         ;; Convert headers
         (h (pregexp-replace* "(?i:<h1[^>]*>([\\s\\S]*?)</h1\\s*>)" h "\n# \\1\n"))
         (h (pregexp-replace* "(?i:<h2[^>]*>([\\s\\S]*?)</h2\\s*>)" h "\n## \\1\n"))
         (h (pregexp-replace* "(?i:<h3[^>]*>([\\s\\S]*?)</h3\\s*>)" h "\n### \\1\n"))
         (h (pregexp-replace* "(?i:<h4[^>]*>([\\s\\S]*?)</h4\\s*>)" h "\n#### \\1\n"))
         (h (pregexp-replace* "(?i:<h5[^>]*>([\\s\\S]*?)</h5\\s*>)" h "\n##### \\1\n"))
         (h (pregexp-replace* "(?i:<h6[^>]*>([\\s\\S]*?)</h6\\s*>)" h "\n###### \\1\n"))
         ;; Convert bold and italic styling
         (h (pregexp-replace* "(?i:<(?:strong|b)\\b[^>]*>([\\s\\S]*?)</(?:strong|b)\\s*>)" h "**\\1**"))
         (h (pregexp-replace* "(?i:<(?:em|i)\\b[^>]*>([\\s\\S]*?)</(?:em|i)\\s*>)" h "*\\1*"))
         ;; Convert links
         (h (pregexp-replace* "(?i:<a\\b[^>]*?\\bhref\\s*=\\s*\"([^\"]*)\"[^>]*>([\\s\\S]*?)</a\\s*>)" h "[\\2](\\1)"))
         (h (pregexp-replace* "(?i:<a\\b[^>]*?\\bhref\\s*=\\s*'([^']*)'[^>]*>([\\s\\S]*?)</a\\s*>)" h "[\\2](\\1)"))
         ;; Convert list items
         (h (pregexp-replace* "(?i:<li[^>]*>([\\s\\S]*?)</li\\s*>)" h "\n- \\1"))
         ;; Convert paragraphs and line breaks
         (h (pregexp-replace* "(?i:<p[^>]*>([\\s\\S]*?)</p\\s*>)" h "\n\n\\1\n\n"))
         (h (pregexp-replace* "(?i:<br\\s*/?>)" h "\n"))
         ;; Strip remaining tags
         (h (pregexp-replace* "<[^>]*>" h " "))
         (h (unescape-html-entities h))
         (h (clean-whitespace h)))
    h))

;; Extract links in document order, resolving relative URLs when base-uri is provided
(def (html->links html (base-uri #f))
  (let* ((pattern "(?i:<a\\b[^>]*?\\bhref\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>\"']+))[^>]*>([\\s\\S]*?)</a\\s*>)")
         (matches (find-all-matches pattern html)))
    (map (lambda (m)
           (let* ((u1 (and (cadr m) (substring html (car (cadr m)) (cdr (cadr m)))))
                  (u2 (and (caddr m) (substring html (car (caddr m)) (cdr (caddr m)))))
                  (u3 (and (cadddr m) (substring html (car (cadddr m)) (cdr (cadddr m)))))
                  (raw-url (or u1 u2 u3 ""))
                  (text-pos (list-ref m 4))
                  (raw-text (if text-pos (substring html (car text-pos) (cdr text-pos)) ""))
                  (url (if base-uri (resolve-url base-uri raw-url) raw-url))
                  (cleaned-text (string-trim (unescape-html-entities (pregexp-replace* "<[^>]*>" raw-text " ")))))
             (list url cleaned-text)))
         matches)))

;; Extract h1, h2, h3 section headers in document order
(def (html->headers html)
  (let* ((pattern "(?i:<h([123])[^>]*>([\\s\\S]*?)</h\\1\\s*>)")
         (matches (find-all-matches pattern html)))
    (map (lambda (m)
           (let* ((level-pos (cadr m))
                  (text-pos (caddr m))
                  (level (string->number (substring html (car level-pos) (cdr level-pos))))
                  (text (substring html (car text-pos) (cdr text-pos)))
                  (cleaned-text (string-trim (unescape-html-entities (pregexp-replace* "<[^>]*>" text " ")))))
             (list level cleaned-text)))
         matches)))

;;; ---- High-level API (fetches then parses) ----------------------------------

(def (get-plain-text uri)
  (html->text (fetch-html uri)))

(def (get-markdown uri)
  (html->markdown (fetch-html uri)))

(def (get-links uri)
  (html->links (fetch-html uri) uri))

(def (get-headers uri)
  (html->headers (fetch-html uri)))
