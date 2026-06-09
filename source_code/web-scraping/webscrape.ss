;; File: webscrape.ss
(import :std/net/request
        :std/text/json
        :std/net/uri
        :std/format
        :std/pregexp)

(export get-plain-text
        get-markdown
        get-links
        get-headers)

;; Fetch raw HTML text from a URI
(def (fetch-html uri)
  (let* ((headers `(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
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

;; Unescape standard HTML entities
(def (unescape-html-entities str)
  (let* ((s (pregexp-replace* "&nbsp;" str " "))
         (s (pregexp-replace* "&amp;" s "&"))
         (s (pregexp-replace* "&lt;" s "<"))
         (s (pregexp-replace* "&gt;" s ">"))
         (s (pregexp-replace* "&quot;" s "\""))
         (s (pregexp-replace* "&#39;" s "'"))
         (s (pregexp-replace* "&apos;" s "'")))
    s))

;; Trim leading and trailing whitespace
(def (string-trim str)
  (pregexp-replace "^\\s+" (pregexp-replace "\\s+$" str "") ""))

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

;; API 1: Get plain text from web URI
(def (get-plain-text uri)
  (let* ((html (fetch-html uri))
         ;; Strip script tags and their content
         (h (pregexp-replace* "<[sS][cC][rR][iI][pP][tT][^>]*>([\\s\\S]*?)</[sS][cC][rR][iI][pP][tT]>" html ""))
         ;; Strip style tags and their content
         (h (pregexp-replace* "<[sS][tT][yY][lL][eE][^>]*>([\\s\\S]*?)</[sS][tT][yY][lL][eE]>" h ""))
         ;; Strip HTML comments
         (h (pregexp-replace* "<!--([\\s\\S]*?)-->" h ""))
         ;; Replace block elements with newlines to avoid text squashing
         (h (pregexp-replace* "<[bB][rR]\\s*/?>|</?[pP]>|</?[dD][iI][vV]>|</?[tT][rR]>|</?[hH][1-6]>" h "\n"))
         ;; Strip all remaining HTML tags
         (h (pregexp-replace* "<[^>]*>" h " "))
         ;; Resolve HTML entities
         (h (unescape-html-entities h))
         ;; Standardize whitespace
         (h (clean-whitespace h)))
    h))

;; API 2: Get Markdown text from URI
(def (get-markdown uri)
  (let* ((html (fetch-html uri))
         ;; Strip script, style, and comments
         (h (pregexp-replace* "<[sS][cC][rR][iI][pP][tT][^>]*>([\\s\\S]*?)</[sS][cC][rR][iI][pP][tT]>" html ""))
         (h (pregexp-replace* "<[sS][tT][yY][lL][eE][^>]*>([\\s\\S]*?)</[sS][tT][yY][lL][eE]>" h ""))
         (h (pregexp-replace* "<!--([\\s\\S]*?)-->" h ""))
         
         ;; Convert headers
         (h (pregexp-replace* "<[hH]1[^>]*>([\\s\\S]*?)</[hH]1>" h "\n# \\1\n"))
         (h (pregexp-replace* "<[hH]2[^>]*>([\\s\\S]*?)</[hH]2>" h "\n## \\1\n"))
         (h (pregexp-replace* "<[hH]3[^>]*>([\\s\\S]*?)</[hH]3>" h "\n### \\1\n"))
         (h (pregexp-replace* "<[hH]4[^>]*>([\\s\\S]*?)</[hH]4>" h "\n#### \\1\n"))
         (h (pregexp-replace* "<[hH]5[^>]*>([\\s\\S]*?)</[hH]5>" h "\n##### \\1\n"))
         (h (pregexp-replace* "<[hH]6[^>]*>([\\s\\S]*?)</[hH]6>" h "\n###### \\1\n"))
         
         ;; Convert bold and italic styling
         (h (pregexp-replace* "<[sS][tT][rR][oO][nN][gG][^>]*>([\\s\\S]*?)</[sS][tT][rR][oO][nN][gG]>" h "**\\1**"))
         (h (pregexp-replace* "<[bB][^>]*>([\\s\\S]*?)</[bB]>" h "**\\1**"))
         (h (pregexp-replace* "<[eE][mM][^>]*>([\\s\\S]*?)</[eE][mM]>" h "*\\1*"))
         (h (pregexp-replace* "<[iI][^>]*>([\\s\\S]*?)</[iI]>" h "*\\1*"))
         
         ;; Convert links
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]=\"([^\"]*)\"[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]='([^']*)'[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]=([^\\s>\"']+)[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         
         ;; Convert list items
         (h (pregexp-replace* "<[lL][iI][^>]*>([\\s\\S]*?)</[lL][iI]>" h "\n- \\1"))
         
         ;; Convert paragraphs and line breaks
         (h (pregexp-replace* "<[pP][^>]*>([\\s\\S]*?)</[pP]>" h "\n\n\\1\n\n"))
         (h (pregexp-replace* "<[bB][rR]\\s*/?>" h "\n"))
         
         ;; Strip any other remaining tags
         (h (pregexp-replace* "<[^>]*>" h " "))
         (h (unescape-html-entities h))
         (h (clean-whitespace h)))
    h))

;; API 3: Get links from URI (returns list of (url text) lists)
(def (get-links uri)
  (let* ((html (fetch-html uri))
         (pattern1 "<[aA][^>]*[hH][rR][eE][fF]=\"([^\"]*)\"[^>]*>([\\s\\S]*?)</[aA]>")
         (pattern2 "<[aA][^>]*[hH][rR][eE][fF]='([^']*)'[^>]*>([\\s\\S]*?)</[aA]>")
         (pattern3 "<[aA][^>]*[hH][rR][eE][fF]=([^\\s>\"']+)[^>]*>([\\s\\S]*?)</[aA]>")
         (matches1 (find-all-matches pattern1 html))
         (matches2 (find-all-matches pattern2 html))
         (matches3 (find-all-matches pattern3 html))
         (all-matches (append matches1 matches2 matches3)))
    (map (lambda (m)
           (let* ((url-pos (cadr m))
                  (text-pos (caddr m))
                  (url (substring html (car url-pos) (cdr url-pos)))
                  (text (substring html (car text-pos) (cdr text-pos)))
                  (cleaned-text (string-trim (unescape-html-entities (pregexp-replace* "<[^>]*>" text " ")))))
             (list url cleaned-text)))
         all-matches)))

;; API 4: Get h1, h2, and h3 section headers (returns list of (level text) lists)
(def (get-headers uri)
  (let* ((html (fetch-html uri))
         (pattern "<[hH]([123])[^>]*>([\\s\\S]*?)</[hH]\\1>")
         (matches (find-all-matches pattern html)))
    (map (lambda (m)
           (let* ((level-pos (cadr m))
                  (text-pos (caddr m))
                  (level (string->number (substring html (car level-pos) (cdr level-pos))))
                  (text (substring html (car text-pos) (cdr text-pos)))
                  (cleaned-text (string-trim (unescape-html-entities (pregexp-replace* "<[^>]*>" text " ")))))
             (list level cleaned-text)))
         matches)))
