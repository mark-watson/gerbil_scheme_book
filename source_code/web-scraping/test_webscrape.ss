;; File: test_webscrape.ss
;; Offline tests for HTML parsing routines using a fixture
(import "webscrape" :std/format)
(export main)

(def sample-html
#<<HTML
<!DOCTYPE html>
<html>
<head>
  <title>Sample Web Page</title>
  <style>body { font-family: sans-serif; }</style>
  <script>console.log("ignore me");</script>
</head>
<body>
  <!-- Header section -->
  <h1>Welcome to Knowledge Graph</h1>
  <div class="intro">
    <p>This is a <strong>sample paragraph</strong> with <em>italic text</em> and a &lt;tag&gt; example.</p>
    <p>Double entity test: &amp;lt; should stay &amp;lt; and not turn into a tag; quote is &#8217;special&#8217;.</p>
  </div>
  <h2>Useful Links</h2>
  <ul>
    <li><a href="https://example.org/docs">Documentation</a></li>
    <li><a href="/about.html">About Page</a></li>
    <li><a href="contact.html">Contact Us</a></li>
  </ul>
  <h3>Footer Information</h3>
  <div>All rights reserved &copy; 2026.</div>
</body>
</html>
HTML
)

(def (assert-equal expected actual label)
  (if (equal? expected actual)
      (begin (displayln "PASS: " label) #t)
      (begin
        (displayln "FAIL: " label)
        (displayln "  Expected: " expected)
        (displayln "  Actual:   " actual)
        (exit 1))))

(def (main . args)
  (displayln "=== Running Web Scraping Offline Unit Tests ===")

  ;; 1. Entity decoding test
  (assert-equal "&lt;" (unescape-html-entities "&amp;lt;") "Double unescape preserves &lt;")
  (assert-equal "It’s" (unescape-html-entities "It&#8217;s") "Decimal numeric entity &#8217;")
  (assert-equal "It's" (unescape-html-entities "It&#x27;s") "Hex numeric entity &#x27;")

  ;; 2. Plain text extraction
  (let ((text (html->text sample-html)))
    (assert-equal #t (and (string-contains text "Welcome to Knowledge Graph") #t) "html->text contains h1")
    (assert-equal #t (not (string-contains text "ignore me")) "html->text stripped script")
    (assert-equal #t (not (string-contains text "font-family")) "html->text stripped style")
    (assert-equal #t (and (string-contains text "<tag>") #t) "html->text unescaped &lt;tag&gt;"))

  ;; 3. Headers extraction
  (let ((headers (html->headers sample-html)))
    (assert-equal 3 (length headers) "Found 3 headers")
    (assert-equal '(1 "Welcome to Knowledge Graph") (car headers) "Header 1 correct")
    (assert-equal '(2 "Useful Links") (cadr headers) "Header 2 correct")
    (assert-equal '(3 "Footer Information") (caddr headers) "Header 3 correct"))

  ;; 4. Links extraction with relative URL resolution
  (let ((links (html->links sample-html "https://example.org/base/index.html")))
    (assert-equal 3 (length links) "Found 3 links")
    (assert-equal '("https://example.org/docs" "Documentation") (car links) "Absolute link in order")
    (assert-equal '("https://example.org/about.html" "About Page") (cadr links) "Root-relative link resolved")
    (assert-equal '("https://example.org/base/contact.html" "Contact Us") (caddr links) "Path-relative link resolved"))

  ;; 5. Markdown extraction
  (let ((md (html->markdown sample-html)))
    (assert-equal #t (and (string-contains md "# Welcome to Knowledge Graph") #t) "Markdown h1")
    (assert-equal #t (and (string-contains md "**sample paragraph**") #t) "Markdown strong/bold")
    (assert-equal #t (and (string-contains md "[Documentation](https://example.org/docs)") #t) "Markdown link"))

  (displayln "All web scraping offline tests passed successfully!"))
