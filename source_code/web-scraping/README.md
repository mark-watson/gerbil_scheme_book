# Web Scraping Library

A minimal, dependency-free Gerbil Scheme library for scraping and extracting structured content from web pages using `:std/net/request` and `:std/pregexp`.

## Features

- **Plain Text Extraction:** Fetches a URI, strips out comments, script blocks, and style blocks, replaces block level tags with newlines, and returns clean plain text.
- **Markdown Conversion:** Parses HTML and converts standard tags (headings `h1`-`h6`, formatting `strong`/`em`/`b`/`i`, lists `li`, line breaks `br`, paragraphs `p`, and links `a`) into their Markdown equivalents.
- **Link Harvesting:** Extracts all hyperlink URLs along with their visible text, returning a list of `(url text)` pairs.
- **Header Parsing:** Extracts `h1`, `h2`, and `h3` tags along with their text contents, returning a list of `(level text)` pairs.

## Files

| File | Description |
|------|-------------|
| `webscrape.ss` | Library module — exports `get-plain-text`, `get-markdown`, `get-links`, and `get-headers` |
| `test_webscrape.ss` | Test program — runs the APIs against `https://markwatson.com` and displays results |
| `gerbil.pkg` | Package declaration (`web-scraping`) |
| `Makefile` | Runs the test program using `gxi` |

## How to run

Simply run `make test` to execute the test suite:

```bash
make test
# or equivalently:
gxi test_webscrape.ss
```

## API Reference

### `(get-plain-text uri)`
Fetches the given `uri` and returns a string containing the text of the webpage with all HTML formatting, script/style blocks, and extra whitespace removed.

### `(get-markdown uri)`
Fetches the given `uri` and converts headings, links, text formatting, lists, and paragraphs into readable Markdown.

### `(get-links uri)`
Fetches the given `uri` and extracts all links. Returns a list of lists of the form `(("url" "text") ...)`.

### `(get-headers uri)`
Fetches the given `uri` and extracts `h1`, `h2`, and `h3` tags. Returns a list of lists of the form `((level "header text") ...)` in order of appearance.
