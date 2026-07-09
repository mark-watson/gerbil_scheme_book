# Web Scraping in Gerbil Scheme

Web scraping is one of the most useful tools in a programmer's toolbox for extracting structured information from the loose, human-oriented format of the World Wide Web. Every time we search a public website for data that has no official API, aggregate news headlines, harvest links for a crawler, or transform an HTML article into a clean corpus for a language model, we are performing a scraping task. In its simplest form, scraping is a three step pipeline: fetch a resource over HTTP, parse the returned HTML, and select the pieces we want.

Industrial scraping libraries (Beautiful Soup in Python, Nokogiri in Ruby, Cheerio in JavaScript) build a full Document Object Model tree and then let you traverse it with CSS or XPath selectors. For many everyday tasks that machinery is overkill. In this chapter we build a small, dependency free Gerbil Scheme library that leans on regular expressions instead of a DOM. The result is compact, fast, and well suited to the kind of quick extraction jobs that come up all the time when working with LLM pipelines, personal knowledge bases, and CLI tooling.

## What We Will Build

The library exposes four public procedures, all of which take a single URI string and return a Scheme value:

| Procedure | Result |
|-----------|--------|
| `get-plain-text` | The visible text of the page as one clean string, with all HTML markup, script blocks, style blocks, and repeated whitespace removed. |
| `get-markdown` | A best effort Markdown rendering of the page, preserving headings, bold and italic emphasis, links, list items, and paragraphs. |
| `get-links` | A list of `(url anchor-text)` pairs for every `<a href="...">` on the page. |
| `get-headers` | A list of `(level text)` pairs for every `<h1>`, `<h2>`, and `<h3>` on the page. |

Each procedure fetches the page independently. The library favors simplicity over caching or connection reuse; if you need to hit the same URL several times in a single script, wrap `fetch-html` yourself.

## Why Regular Expressions and Not a Real Parser

The classic warning "you cannot parse HTML with regular expressions" is essentially true if your goal is to build a general purpose browser. Real world HTML is riddled with unclosed tags, nested quirks, and layered inline scripts that only a proper tokenizer plus tree builder can handle correctly. So why do we lean on regular expressions here?

- The scraping tasks in this chapter are **extraction** tasks, not **rendering** tasks. We are pulling specific fragments (all links, all H2 titles, the visible text) and can tolerate the occasional edge case.
- Gerbil ships with `:std/pregexp`, a Perl compatible regex engine. Nothing else is needed. This keeps the library dependency free and portable.
- Regex based extraction is fast and easy to reason about. Each pass is one pattern substitution.

The tradeoff is that we will fail on pathological input (nested comments containing fake `</script>` tags, for example). For personal and educational scraping tasks that tradeoff is very acceptable.

## Project Structure

The project directory `source_code/web-scraping` is small:

| File | Description |
|------|-------------|
| `webscrape.ss` | The library module that exports the four public procedures. |
| `test_webscrape.ss` | A test driver that exercises all four procedures against `https://markwatson.com`. |
| `gerbil.pkg` | Package declaration (`web-scraping`). |
| `Makefile` | A single `test` target that runs the test driver with `gxi`. |

The package declaration is a single line:

```scheme
(package: web-scraping)
```

And the Makefile is equally minimal:

```makefile
test:
	gxi test_webscrape.ss
```

## The Library: webscrape.ss

The module imports the standard networking, JSON, URI, formatting, and regex libraries, then declares the four public procedures we will build:

```scheme
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
```

### Fetching HTML

Every scraping procedure begins by fetching the raw HTML for a URL. The `fetch-html` helper wraps Gerbil's `http-get` and sets a browser like `User-Agent` header. Many sites reject requests that come in with the default Gerbil user agent, so identifying as a recent Chrome build gets us cleanly past most heuristic filters:

```scheme
;; Fetch raw HTML text from a URI
(def (fetch-html uri)
  (let* ((headers `(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
                    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")))
         (response (http-get uri headers: headers)))
    (if (= (request-status response) 200)
      (request-text response)
      (error "Failed to fetch URI" status: (request-status response) uri: uri))))
```

We insist on HTTP status `200`. Anything else (a redirect, a client error, a server error) raises an exception rather than silently returning a partial or empty page. In production code you would probably want to follow `301` and `302` redirects, but Gerbil's `http-get` handles the common cases for us.

### Enumerating All Regex Matches

The `pregexp-match-positions` primitive returns positional information for the first match starting at a given index. To collect every match in the page we drive it in a loop, advancing the search cursor past each hit. The `find-all-matches` helper does exactly that and also protects against the classic zero width match trap by forcing a single character step whenever the match end equals the search start:

```scheme
;; Find all regex matches in a string and return positional info
(def (find-all-matches pattern str)
  (let loop ((start 0) (acc '()))
    (let ((pos (pregexp-match-positions pattern str start)))
      (if pos
        (let* ((match-end (cdr (car pos)))
               (next-start (if (= match-end start) (+ start 1) match-end)))
          (loop next-start (cons pos acc)))
        (reverse acc)))))
```

The helper returns matches in document order, each match represented as a list of `(start . end)` pairs where the first pair is the full match and each subsequent pair is a capturing group.

### Unescaping HTML Entities

HTML text is peppered with named and numeric character references such as `&amp;`, `&nbsp;`, and `&#39;`. Once we have stripped the tags, we still need to translate these back into their plain characters. `unescape-html-entities` handles the small handful that appear most often on real pages:

```scheme
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
```

If you plan to scrape pages with heavy use of numeric character references (`&#0123;`, `&#x1F600;`, and so on) you would want to extend this table. For English text on modern sites, the seven substitutions above cover almost every real world case.

### Cleaning Whitespace

HTML source is indented, wrapped, and typically contains a great deal of runs of blanks that translate poorly to plain text output. `clean-whitespace` runs a small pipeline of regex passes to normalize the result:

```scheme
;; Trim leading and trailing whitespace
(def (string-trim str)
  (pregexp-replace "^\\s+" (pregexp-replace "\\s+$" str "") ""))

;; Clean up consecutive whitespace and newlines
(def (clean-whitespace str)
  (let* ((s (pregexp-replace* "\r" str ""))
         (s (pregexp-replace* "[ \t]+\n" s "\n"))
         (s (pregexp-replace* "\n[ \t]+" s "\n"))
         (s (pregexp-replace* "\n{3,}" s "\n\n"))
         (s (pregexp-replace* "[ \t]+" s " "))
         (s (string-trim s)))
    s))
```

The five substitutions do the following:

1. Delete stray carriage returns (Windows line endings).
2. Trim trailing spaces and tabs off each line.
3. Trim leading spaces and tabs off each line.
4. Collapse three or more consecutive newlines down to exactly two, preserving paragraph breaks but no more.
5. Collapse runs of spaces or tabs into a single space.

Finally the outer whole string is trimmed. The result is text that reads well when dumped straight to the terminal.

### API 1: Plain Text Extraction

`get-plain-text` is the simplest of the four public procedures. Its job is to return the reader visible text of the page with all HTML markup gone. The strategy is a cascade of regex substitutions:

```scheme
;; API 1: Get plain text from web URI
(def (get-plain-text uri)
  (let* ((html (fetch-html uri))
         (h (pregexp-replace* "<[sS][cC][rR][iI][pP][tT][^>]*>([\\s\\S]*?)</[sS][cC][rR][iI][pP][tT]>" html ""))
         (h (pregexp-replace* "<[sS][tT][yY][lL][eE][^>]*>([\\s\\S]*?)</[sS][tT][yY][lL][eE]>" h ""))
         (h (pregexp-replace* "<!--([\\s\\S]*?)-->" h ""))
         (h (pregexp-replace* "<[bB][rR]\\s*/?>|</?[pP]>|</?[dD][iI][vV]>|</?[tT][rR]>|</?[hH][1-6]>" h "\n"))
         (h (pregexp-replace* "<[^>]*>" h " "))
         (h (unescape-html-entities h))
         (h (clean-whitespace h)))
    h))
```

Reading top to bottom:

1. Strip `<script>...</script>` blocks including their content. This is essential; otherwise inline JavaScript source ends up in the "text".
2. Strip `<style>...</style>` blocks likewise.
3. Strip HTML comments.
4. Replace block level tags (`<br>`, `<p>`, `<div>`, `<tr>`, `<h1>` through `<h6>`) with newlines so paragraphs stay separated.
5. Strip every remaining tag, replacing it with a single space so words on either side stay separated.
6. Unescape HTML entities.
7. Normalize whitespace.

The character class `[sS][cC][rR][iI][pP][tT]` is a case insensitive match for "script". Gerbil's `pregexp` supports the `(?i:...)` case insensitive flag as well, but the explicit character class form makes the pattern's intent obvious at a glance.

### API 2: Markdown Conversion

`get-markdown` returns a Markdown formatted rendering of the page. It stitches together the same "strip scripts and styles" preamble as `get-plain-text`, then applies additional patterns to convert HTML tags into their Markdown equivalents before stripping any remaining tags:

```scheme
;; API 2: Get Markdown text from URI
(def (get-markdown uri)
  (let* ((html (fetch-html uri))
         (h (pregexp-replace* "<[sS][cC][rR][iI][pP][tT][^>]*>([\\s\\S]*?)</[sS][cC][rR][iI][pP][tT]>" html ""))
         (h (pregexp-replace* "<[sS][tT][yY][lL][eE][^>]*>([\\s\\S]*?)</[sS][tT][yY][lL][eE]>" h ""))
         (h (pregexp-replace* "<!--([\\s\\S]*?)-->" h ""))
         
         (h (pregexp-replace* "<[hH]1[^>]*>([\\s\\S]*?)</[hH]1>" h "\n# \\1\n"))
         (h (pregexp-replace* "<[hH]2[^>]*>([\\s\\S]*?)</[hH]2>" h "\n## \\1\n"))
         (h (pregexp-replace* "<[hH]3[^>]*>([\\s\\S]*?)</[hH]3>" h "\n### \\1\n"))
         (h (pregexp-replace* "<[hH]4[^>]*>([\\s\\S]*?)</[hH]4>" h "\n#### \\1\n"))
         (h (pregexp-replace* "<[hH]5[^>]*>([\\s\\S]*?)</[hH]5>" h "\n##### \\1\n"))
         (h (pregexp-replace* "<[hH]6[^>]*>([\\s\\S]*?)</[hH]6>" h "\n###### \\1\n"))
         
         (h (pregexp-replace* "<[sS][tT][rR][oO][nN][gG][^>]*>([\\s\\S]*?)</[sS][tT][rR][oO][nN][gG]>" h "**\\1**"))
         (h (pregexp-replace* "<[bB][^>]*>([\\s\\S]*?)</[bB]>" h "**\\1**"))
         (h (pregexp-replace* "<[eE][mM][^>]*>([\\s\\S]*?)</[eE][mM]>" h "*\\1*"))
         (h (pregexp-replace* "<[iI][^>]*>([\\s\\S]*?)</[iI]>" h "*\\1*"))
         
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]=\"([^\"]*)\"[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]='([^']*)'[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         (h (pregexp-replace* "<[aA][^>]*[hH][rR][eE][fF]=([^\\s>\"']+)[^>]*>([\\s\\S]*?)</[aA]>" h "[\\2](\\1)"))
         
         (h (pregexp-replace* "<[lL][iI][^>]*>([\\s\\S]*?)</[lL][iI]>" h "\n- \\1"))
         
         (h (pregexp-replace* "<[pP][^>]*>([\\s\\S]*?)</[pP]>" h "\n\n\\1\n\n"))
         (h (pregexp-replace* "<[bB][rR]\\s*/?>" h "\n"))
         
         (h (pregexp-replace* "<[^>]*>" h " "))
         (h (unescape-html-entities h))
         (h (clean-whitespace h)))
    h))
```

Two observations are worth calling out.

First, the three anchor patterns handle the three ways HTML authors write the `href` attribute: double quoted, single quoted, and unquoted. Real world pages contain all three, and coalescing them into a single pattern is more trouble than it is worth.

Second, the ordering of substitutions is deliberate. Headings, emphasis, and anchors are converted **before** the catch all `<[^>]*>` tag stripper runs. If we had run the stripper first, we would have lost the URLs inside the anchor tags.

### API 3: Extracting Links

`get-links` scans the entire HTML for `<a href="...">` tags and returns a list of `(url text)` pairs. The `find-all-matches` helper does the heavy lifting; we simply run it three times, once per quoting style, and concatenate the results:

```scheme
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
```

Each match `m` returned by `find-all-matches` is a list where `car` is the full match's `(start . end)` and each `cadr`, `caddr`, `...` is a capturing group's `(start . end)`. We use `substring` on the original HTML to pull out the captured URL and anchor text. The anchor text is then cleaned by stripping any inner tags (`<span>`, `<img>`, and so on) and unescaping HTML entities.

### API 4: Extracting Headers

`get-headers` returns a flat list of `(level text)` pairs for every H1, H2, and H3 tag. The pattern uses a backreference (`\\1`) so that the closing tag matches the opening tag's level:

```scheme
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
```

The captured level (`"1"`, `"2"`, or `"3"`) is converted to an integer with `string->number`. The captured text is cleaned in the same way as anchor text. The returned list preserves the order in which the headings appear in the source, which is exactly what you want when reconstructing a table of contents.

## The Test Program: test_webscrape.ss

The test driver exercises all four APIs against `https://markwatson.com`. It imports the sibling module by filename (no leading colon) and calls each procedure in turn, printing a truncated preview so the output remains manageable:

```scheme
;; File: test_webscrape.ss
(import "webscrape")
(import :std/format)

(def (run-tests)
  (let ((uri "https://markwatson.com"))
    (displayln "=============================================")
    (displayln "1. TESTING PLAIN TEXT EXTRACTION FROM: " uri)
    (displayln "=============================================")
    (let ((text (get-plain-text uri)))
      (displayln (substring text 0 (min (string-length text) 600)))
      (displayln "\n... [truncated] ...\n"))

    (displayln "=============================================")
    (displayln "2. TESTING MARKDOWN EXTRACTION FROM: " uri)
    (displayln "=============================================")
    (let ((md (get-markdown uri)))
      (displayln (substring md 0 (min (string-length md) 600)))
      (displayln "\n... [truncated] ...\n"))

    (displayln "=============================================")
    (displayln "3. TESTING HEADERS EXTRACTION (H1, H2, H3) FROM: " uri)
    (displayln "=============================================")
    (let ((headers (get-headers uri)))
      (for-each (lambda (h)
                  (let ((level (car h))
                        (text (cadr h)))
                    (displayln (make-string level #\#) " " text)))
                headers)
      (displayln ""))

    (displayln "=============================================")
    (displayln "4. TESTING LINKS EXTRACTION FROM: " uri)
    (displayln "=============================================")
    (let ((links (get-links uri)))
      (displayln "Found " (length links) " links. Printing the first 15:")
      (let loop ((count 0) (lst links))
        (when (and (< count 15) (not (null? lst)))
          (let* ((link (car lst))
                 (url (car link))
                 (text (cadr link)))
            (displayln (format "  ~a. [~a] -> ~a" (+ count 1) text url))
            (loop (+ count 1) (cdr lst)))))
      (displayln ""))))

(run-tests)
```

The `substring text 0 (min (string-length text) 600)` idiom is a defensive truncation. If the fetched page turns out to be shorter than 600 characters, using a plain `substring` with hardcoded `600` would raise an out of range error.

## Running the Test Driver

Assuming Gerbil is installed and network access is available, run:

```console
$ make test
gxi test_webscrape.ss
```

Here is an edited output from my web site `https://markwatson.com`. Much of the output is removed here for brevity:

```console
$ make
gxi test_webscrape.ss
=============================================
1. TESTING PLAIN TEXT EXTRACTION FROM: https://markwatson.com
=============================================
Mark Watson — Open Source Artificial Intelligence Research

Independent open source Artificial Intelligence research. Author of 20+ books, holder of 55 US patents, with over 40 years of experience helping organizations like Google , Capital One , Disney , SAIC , and CompassLabs .

Books
Read My Books — Many for Free

All of my recent books are available on Leanpub. Most can be read online at no cost.

Practical Python Artificial Inte

... [truncated] ...

=============================================
2. TESTING MARKDOWN EXTRACTION FROM: https://markwatson.com
=============================================
Mark Watson — Open Source Artificial Intelligence Research

- [Blogspot](https://mark-watson.blogspot.com/)

- [Substack](https://marklwatson.substack.com)

- [GitHub](https://github.com/mark-watson)

- [Leanpub](https://leanpub.com/u/markwatson)

# Mark Watson .

Independent open source Artificial Intelligence research. Author of 20+ books, holder of 55 US patents, with over 40 years of experience helping organizations like **Google**, **Capital One**, **Disney**, **SAIC**, and **CompassLabs**.

... [truncated] ...

=============================================
3. TESTING HEADERS EXTRACTION (H1, H2, H3) FROM: https://markwatson.com
=============================================
# Mark Watson .
## Read My Books — Many for Free
## Connect & Interests
### Writings & Blogs
### Open Source
### Consulting
### Interests & Music

=============================================
4. TESTING LINKS EXTRACTION FROM: https://markwatson.com
=============================================
Found 46 links. Printing the first 15:
  1. [Mark Watson] -> #
  2. [Books] -> #books
  3. [Connect] -> #connect
  4. [Blogspot] -> https://mark-watson.blogspot.com/
  5. [Substack] -> https://marklwatson.substack.com
  6. [GitHub] -> https://github.com/mark-watson
  7. [Leanpub] -> https://leanpub.com/u/markwatson
  8. [Explore My Books] -> #books
  9. [Connect with Me] -> #connect
  10. [Read Free ↗] -> https://leanpub.com/pythonai/read
  11. [Purchase] -> https://leanpub.com/pythonai
  12. [Read Free ↗] -> https://leanpub.com/lovinglisp/read
  13. [Purchase] -> https://leanpub.com/lovinglisp
  14. [Read Free ↗] -> https://leanpub.com/clojureai/read
  15. [Purchase] -> https://leanpub.com/clojureai
```

## Interpreting the Output

Look at what each of the four sections tells us.

The **plain text extraction** shows that our regex based tag stripping does its job. Every `<script>` block (there is usually at least an analytics tag on any modern site) has been suppressed, every inline style has been dropped, and the visible copy has been arranged into paragraphs by the block level tag substitution. The result is clean enough to feed directly into a large language model as context.

The **Markdown extraction** shows the same content in a form that preserves structure. Headings appear as `# ...`, `## ...`, list items as `- ...`, and links as `[text](url)`. This form is ideal when you want to render the content downstream or store it in a note taking system that speaks Markdown.

The **headers extraction** gives us a table of contents style summary in about ten lines. This is useful for building navigational overlays, generating sitemaps, or feeding a summarizer that benefits from knowing the document's outline.

The **links extraction** returns 47 links in this particular run. In practice you would filter this list further: keep only external links, only links whose text is not the URL itself, only links to a specific domain, and so on. Because the return value is a plain list of Scheme values, all of these filters are one-liners with `filter` and `map`.

A subtle but important thing to notice is that the returned links include relative URLs (`/contact/`, `/privacy/`). If you plan to follow those links you must resolve them against the base URI. Gerbil's `:std/net/uri` module (imported at the top of the file but not used in this iteration) provides `uri-merge` for exactly that.

## Design Choices and Limitations

Building a small scraping toolkit forces you to face several design tradeoffs. The choices we made here are worth stating explicitly.

- **No JavaScript execution.** We only see the HTML the server sends. Sites that populate their content at runtime with client side JavaScript (a growing category) will look almost empty to us. For those pages you would need to drive a headless browser, which is out of scope for a lightweight library.
- **No cookie or session handling.** Every fetch is independent. Login walls and multi step flows are not supported.
- **No robots.txt awareness.** A well behaved scraper checks `robots.txt` before hitting a site. Adding that check is a good exercise (see the practice problems below).
- **No rate limiting.** If you loop over hundreds of URLs, add your own delay. A friendly convention is to wait a second or two between requests to the same host.
- **Regex fragility.** Any page that includes literal `<a href="...">` text inside a code block will confuse `get-links`. In practice, this happens rarely on public web pages.

Despite these limitations, the four public procedures cover a surprising fraction of everyday scraping tasks, especially those associated with feeding curated web content into LLM based systems.

## Wrap Up

Web scraping in Gerbil Scheme with only `:std/net/request` and `:std/pregexp` is genuinely enjoyable. The library we assembled fits on one screen, has no external dependencies, and provides four useful transformations of a web page: plain text, Markdown, links, and headers. The design leans on Gerbil's Perl compatible regex engine and its threaded string transformations, so each stage of the pipeline is a short, self contained substitution.

This library pairs naturally with the other components of a personal AI toolkit: fetch a page, convert it to Markdown, and feed the result into a Gemini or Ollama call. Or scrape the headers of a documentation site to build a keyword index. Because the results are just Scheme lists and strings, the output composes freely with everything else in your program.

## Optional Practice Problems

1. **Follow Redirects and Handle HTTPS Errors Gracefully.** `fetch-html` currently raises an error on any non 200 status. Extend it to follow `301` and `302` redirects up to a small maximum (say, five hops) and to return an empty string when the final response is a `4xx` or `5xx` so that a batch scraping job does not abort on a single bad URL.

2. **Add Image Extraction.** Write a new procedure `get-images` that returns a list of `(url alt-text)` pairs for every `<img src="..." alt="...">` on the page. Handle single quoted, double quoted, and unquoted `src` attributes just like `get-links` does for `href`.

3. **Resolve Relative URLs.** Modify `get-links` (and your new `get-images`) so that URLs that begin with `/`, `./`, or `../` are resolved against the base URI using `uri-merge` from `:std/net/uri`. Return absolute URLs only.

4. **Extract Structured Metadata.** Many pages include Open Graph tags (`<meta property="og:title" content="...">`) and Twitter card tags. Write `get-metadata` that returns an association list mapping property names to their `content` values. Test it against a news article to confirm you get the title, description, image, and canonical URL.

5. **Respect robots.txt.** Write a procedure `robots-allowed?` that fetches `/robots.txt` from the target host, parses the relevant `User-agent` sections, and returns `#t` or `#f` for a given URL path. Use it to guard each call in a batch scraper.

6. **Concurrent Batch Scraping.** Given a list of URLs, use Gerbil's threading primitives (`spawn`, `thread-join!`) to fetch them in parallel, up to a configurable maximum concurrency. Return a hash table mapping each URL to its extracted plain text.

7. **Feed Scraped Content into an LLM.** Combine `get-markdown` with an existing Gemini or Ollama client from earlier chapters. Write a small script that takes a URL and a question on the command line, scrapes the URL, and asks the LLM to answer the question using only the scraped content as context.

8. **Convert Tables to Markdown.** Extend `get-markdown` to recognise `<table>`, `<tr>`, `<th>`, and `<td>` and emit valid Markdown tables. Test it on a page that contains a data table (a Wikipedia infobox is a good target).
