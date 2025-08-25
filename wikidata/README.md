Wikidata SPARQL (Gerbil Scheme)

Overview
- Small Gerbil Scheme helper to query the Wikidata Query Service (WDQS) from Scheme and parse SPARQL JSON results.
- Replaces a prior DBpedia-only version; `query-dbpedia` remains as an alias to `query-wikidata` for compatibility.

Requirements
- Gerbil Scheme (gxi/gxc)

Quick Start
- Run the example query: `make test`
- Or evaluate manually:
  - `gxi -L wikidata.ss -e "(test2)"`
 - With a User-Agent: `make run-agent` (uses `WDQS_UA` or a default sample)

API
- `query-wikidata query [user-agent]`
  - Sends `query` to `https://query.wikidata.org/sparql` using GET.
  - Returns a list of hash tables: each row maps symbols to string values.
  - Optional `user-agent` string is recommended to comply with WDQS policy.

- `query-wikidata/alist query [user-agent]`
  - Same as above but returns each row as an alist: `((var . value) ...)`.

- `alist-ref key row [default]`
  - Convenience accessor for alist rows. `key` can be a symbol or string.
  - Returns the value or `default` (or `#f` if omitted).

- `query-dbpedia query [user-agent]`
  - Backward-compatibility alias that simply calls `query-wikidata`.

Example (Grace Hopper)
```
(import "wikidata.ss")

(def query
  (string-append
   "PREFIX wd: <http://www.wikidata.org/entity/>\n"
   "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
   "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
   "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
   "SELECT ?birthDate ?birthPlaceLabel WHERE {\n"
   "  wd:Q7249 wdt:P569 ?birthDate .\n"
   "  wd:Q7249 wdt:P19 ?birthPlace .\n"
   "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
   "}"))

(def rows (query-wikidata/alist query "YourApp/1.0 (https://your.site; you@site)"))

(for-each
 (lambda (row)
   (displayln (string-append "Birth Date: " (or (alist-ref 'birthDate row) "")))
   (displayln (string-append "Birth Place: " (or (alist-ref 'birthPlaceLabel row) "")))
   (displayln ""))
 rows)
```

Notes
- User-Agent: WDQS requires a descriptive `User-Agent` with contact info; please set it for anything beyond quick local testing.
- Rate limits: Be considerate of WDQS usage policies. Prefer caching, smaller result sets, and avoid rapid-fire queries.
- Large queries: If your queries exceed URL length, consider adding a POST variant using `application/x-www-form-urlencoded` with `query=...`. (Open an issue if you want this built-in.)

Make Targets
- `test`: runs the Grace Hopper example without specifying a User-Agent.
- `run-agent`: runs the same example but reads `WDQS_UA` from the environment; if unset, a sample UA is used.
  - Example: `WDQS_UA="MyTool/0.1 (https://my.site; me@site)" make run-agent`
 - `test1`: finds URIs for Bill Gates and Microsoft, then prints predicates linking them in both directions.
 - `run-agent1`: same as `test1` but honors `WDQS_UA` for a proper User-Agent string.
