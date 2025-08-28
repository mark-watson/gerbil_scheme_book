# Wikidata API Using SPARQL Queries

Wikidata is a free, collaborative, and multilingual knowledge base that functions as the central structured data repository for the Wikimedia ecosystem, including projects like Wikipedia, Wikivoyage, and Wiktionary. Launched in 2012, its mission is to create a common source of open data that can be used by anyone, anywhere. Unlike Wikipedia, which contains prose articles, Wikidata stores information in a machine-readable format structured around items (representing any concept or object), which are described by properties (like "population" or "author") and corresponding values. For example, the item for "Earth" has a property "instance of" with the value "planet". This structured approach allows for data consistency across hundreds of language editions of Wikipedia and enables powerful, complex queries through its SPARQL endpoint. By providing a centralized, queryable, and interlinked database of facts, Wikidata not only supports Wikimedia projects but also serves as a crucial resource for researchers, developers, and applications worldwide that require reliable and openly licensed structured information.


## Example Code

This code in file **wikidata.ss** provides a client for interacting with the Wikidata Query Service, a powerful public SPARQL endpoint for accessing the vast, collaboratively edited knowledge base of Wikidata. The code encapsulates the entire process of querying this service, starting with a raw SPARQL query string. It properly formats the request by URL-encoding the query, constructs the full request URL, and sets the appropriate HTTP headers, including the Accept header for the SPARQL JSON results format and a User-Agent header, which is a requirement for responsible API usage. Upon receiving a response, the module parses the JSON data and then transforms the verbose, nested structure of the standard SPARQL results format into a more convenient and idiomatic Gerbil Scheme data structure—either a list of hash tables or a list of association lists. The file also includes several example functions that demonstrate how to query for specific facts, such as Grace Hopper's birth date, and how to perform more complex, multi-stage queries, like first finding the unique identifiers for entities like Bill Gates and Microsoft and then discovering the relationships that connect them within the knowledge graph.

{lang="scheme", linenos=off}
```
;; File: wikidata.ss
(import :std/net/request
        :std/text/json
        :std/net/uri) ; For URL encoding
(import :std/format)

(export query-wikidata query-wikidata/alist query-dbpedia alist-ref test1 test1-ua test2 test2-ua)

;; Helper to process the SPARQL JSON results format
(def (process-sparql-results json-data)
  (let* ((results (hash-ref json-data 'results))
         (bindings (hash-ref results 'bindings)))
    (map (lambda (binding)
           (let ((result-hash (make-hash-table)))
             (hash-for-each
              (lambda (var-name value-obj)
                (hash-put! result-hash
                           (if (symbol? var-name) var-name (string->symbol var-name))
                           (hash-ref value-obj 'value)))
              binding)
             result-hash))
         bindings)))

;; Convenience: look up a key in an alist. Accepts symbol or string keys.
;; Usage: (alist-ref 'var row [default])
(def (alist-ref key row . default)
  (let* ((sym (if (symbol? key) key (string->symbol key)))
         (p (assq sym row)))
    (if p (cdr p) (if (pair? default) (car default) #f))))

;; Same as above but returns an alist per row: ((var . value) ...)
(def (process-sparql-results-alist json-data)
  (let* ((results (hash-ref json-data 'results))
         (bindings (hash-ref results 'bindings)))
    (map (lambda (binding)
           (let ((row '()))
             (hash-for-each
              (lambda (var-name value-obj)
                (let* ((sym (if (symbol? var-name) var-name (string->symbol var-name)))
                       (val (hash-ref value-obj 'value)))
                  (set! row (cons (cons sym val) row))))
              binding)
             (reverse row)))
         bindings)))

;; Query the Wikidata Query Service (WDQS)
;; - Uses GET with URL-encoded query and JSON format
;; - Sends a User-Agent per WDQS guidelines; callers can override
(def (query-wikidata sparql-query . opts)
  (let* ((endpoint "https://query.wikidata.org/sparql")
         (encoded-query (uri-encode sparql-query))
         (request-url (string-append endpoint "?query=" encoded-query "&format=json"))
         (user-agent (if (pair? opts) (car opts) "gerbil-wikidata/0.1 (+https://example.org; contact@example.org)"))
         (headers `(("Accept" . "application/sparql-results+json")
                    ("User-Agent" . ,user-agent))))
    (let ((response (http-get request-url headers: headers)))
      (if (= (request-status response) 200)
          (let ((response-json (request-json response)))
            (process-sparql-results response-json))
          (error "SPARQL query failed"
                 status: (request-status response)
                 body: (request-text response))))))

;; Alist variant returning rows as association lists
(def (query-wikidata/alist sparql-query . opts)
  (let* ((endpoint "https://query.wikidata.org/sparql")
         (encoded-query (uri-encode sparql-query))
         (request-url (string-append endpoint "?query=" encoded-query "&format=json"))
         (user-agent (if (pair? opts) (car opts) "gerbil-wikidata/0.1 (+https://example.org; contact@example.org)"))
         (headers `(("Accept" . "application/sparql-results+json")
                    ("User-Agent" . ,user-agent))))
    (let ((response (http-get request-url headers: headers)))
      (if (= (request-status response) 200)
          (let ((response-json (request-json response)))
            (process-sparql-results-alist response-json))
          (error "SPARQL query failed"
                 status: (request-status response)
                 body: (request-text response))))))

;; Backward-compatibility alias for previous DBPedia function name
(def (query-dbpedia . args)
  (apply query-wikidata args))

;; Example Usage: fetch birth date and birthplace label for Grace Hopper
(def (test2)
  (let ((query
         (string-append
          "PREFIX wd: <http://www.wikidata.org/entity/>\n"
          "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
          "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
          "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
          "SELECT ?birthDate ?birthPlaceLabel WHERE {\n"
          "  wd:Q7249 wdt:P569 ?birthDate .\n"
          "  wd:Q7249 wdt:P19 ?birthPlace .\n"
          "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
          "}")))
    (let ((results (query-wikidata/alist query)))
      (for-each
       (lambda (result)
         (display (format "Birth Date: ~a\n" (alist-ref 'birthDate result)))
         (display (format "Birth Place: ~a\n\n" (alist-ref 'birthPlaceLabel result))))
       results))))

;; Test1: find URIs for Bill Gates and Microsoft; then list relationships
(def (test1)
  (let* ((find-uris
          (string-append
           "PREFIX wd: <http://www.wikidata.org/entity/>\n"
           "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
           "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
           "SELECT ?bill ?microsoft WHERE {\n"
           "  ?bill rdfs:label \"Bill Gates\"@en .\n"
           "  ?bill wdt:P31 wd:Q5 .\n"
           "  ?microsoft rdfs:label \"Microsoft\"@en .\n"
           ;; Ensure we pick the company entity
           "  ?microsoft wdt:P31/wdt:P279* wd:Q4830453 .\n"
           "} LIMIT 1"))
         (rows (query-wikidata/alist find-uris)))
    (if (null? rows)
        (display "No URIs found for Bill Gates/Microsoft.\n")
        (let* ((row (car rows))
               (bill (alist-ref 'bill row))
               (microsoft (alist-ref 'microsoft row))
               (rel-query
                (string-append
                 "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
                 "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
                 "SELECT ?prop ?propLabel ?dir WHERE {\n"
                 "  VALUES (?bill ?microsoft) { (<" bill "> <" microsoft ">) }\n"
                 "  ?wdprop wikibase:directClaim ?prop .\n"
                 "  { BIND(\"Bill->Microsoft\" AS ?dir) ?bill ?prop ?microsoft . }\n"
                 "  UNION\n"
                 "  { BIND(\"Microsoft->Bill\" AS ?dir) ?microsoft ?prop ?bill . }\n"
                 "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
                 "}\n"
                 "ORDER BY ?propLabel"))
               (rels (query-wikidata/alist rel-query)))
          (display (format "Bill Gates URI: ~a\n" bill))
          (display (format "Microsoft URI: ~a\n" microsoft))
          (if (null? rels)
              (display "No direct relationships found.\n")
              (for-each
               (lambda (r)
                 (display (format "~a: ~a\n"
                                  (alist-ref 'dir r)
                                  (or (alist-ref 'propLabel r)
                                      (alist-ref 'prop r)))))
               rels))))))

;; Test1 with User-Agent from env var WDQS_UA
(def (test1-ua)
  (let* ((ua (or (getenv "WDQS_UA")
                 "YourApp/1.0 (https://your.site; you@site)"))
         (find-uris
          (string-append
           "PREFIX wd: <http://www.wikidata.org/entity/>\n"
           "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
           "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
           "SELECT ?bill ?microsoft WHERE {\n"
           "  ?bill rdfs:label \"Bill Gates\"@en .\n"
           "  ?bill wdt:P31 wd:Q5 .\n"
           "  ?microsoft rdfs:label \"Microsoft\"@en .\n"
           "  ?microsoft wdt:P31/wdt:P279* wd:Q4830453 .\n"
           "} LIMIT 1"))
         (rows (query-wikidata/alist find-uris ua)))
    (if (null? rows)
        (display "No URIs found for Bill Gates/Microsoft.\n")
        (let* ((row (car rows))
               (bill (alist-ref 'bill row))
               (microsoft (alist-ref 'microsoft row))
               (rel-query
                (string-append
                 "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
                 "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
                 "SELECT ?prop ?propLabel ?dir WHERE {\n"
                 "  VALUES (?bill ?microsoft) { (<" bill "> <" microsoft ">) }\n"
                 "  ?wdprop wikibase:directClaim ?prop .\n"
                 "  { BIND(\"Bill->Microsoft\" AS ?dir) ?bill ?prop ?microsoft . }\n"
                 "  UNION\n"
                 "  { BIND(\"Microsoft->Bill\" AS ?dir) ?microsoft ?prop ?bill . }\n"
                 "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
                 "}\n"
                 "ORDER BY ?propLabel"))
               (rels (query-wikidata/alist rel-query ua)))
          (display (format "Bill Gates URI: ~a\n" bill))
          (display (format "Microsoft URI: ~a\n" microsoft))
          (if (null? rels)
              (display "No direct relationships found.\n")
              (for-each
               (lambda (r)
                 (display (format "~a: ~a\n"
                                  (alist-ref 'dir r)
                                  (or (alist-ref 'propLabel r)
                                      (alist-ref 'prop r)))))
               rels))))))

;; Example Usage with User-Agent from env var WDQS_UA
(def (test2-ua)
  (let* ((ua (or (getenv "WDQS_UA")
                 "YourApp/1.0 (https://your.site; you@site)"))
         (query
          (string-append
           "PREFIX wd: <http://www.wikidata.org/entity/>\n"
           "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
           "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
           "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
           "SELECT ?birthDate ?birthPlaceLabel WHERE {\n"
           "  wd:Q7249 wdt:P569 ?birthDate .\n"
           "  wd:Q7249 wdt:P19 ?birthPlace .\n"
           "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
           "}")))
    (let ((results (query-wikidata/alist query ua)))
      (for-each
       (lambda (result)
         (display (format "Birth Date: ~a\n" (alist-ref 'birthDate result)))
         (display (format "Birth Place: ~a\n\n" (alist-ref 'birthPlaceLabel result))))
       results))))
```

The core of this example is built around the **query-wikidata** and **query-wikidata/alist** functions. These procedures handle the low-level details of HTTP communication with the Wikidata SPARQL endpoint. They take a SPARQL query string, URI-encode it, and embed it into a GET request URL. They set a User-Agent string, a best practice that helps service operators identify the source of traffic; the functions allow this header to be overridden via an optional argument. After a successful request, the real data processing begins. The raw JSON response from a SPARQL endpoint is deeply nested, with each result variable wrapped in an object containing its type and value. The helper functions **process-sparql-results** and **process-sparql-results-alist** traverse this structure to extract the essential value of each binding, returning a clean list of results where each result is either a hash table or an association list mapping variable names (as symbols) to their values.

The included test functions, **test1** and **test2**, serve as practical examples of this code's capabilities. The **test2** function is a straightforward lookup, retrieving the birth date and birthplace for a specific Wikidata entity (Grace Hopper, wd:Q7249). In contrast, **test1** demonstrates a more powerful, dynamic pattern. It first runs a query to find the URIs for "Bill Gates" and "Microsoft" based on their English labels and types (human and business, respectively). It then uses these dynamically discovered URIs to construct a second query that finds all direct properties linking the two entities. This two-step approach is a common and robust method for interacting with linked data systems. Furthermore, the test1-ua and test2-ua variants illustrate how to provide a custom User-Agent string, for instance by reading it from an environment variable, showcasing the flexibility of the primary query functions.

## Example Output

We use a Makefile target to start a Gerbil Scheme REPL with the example code loaded:

Here is the Makefile:

```makefile
$ cat Makefile 
run:
	gxi -L wikidata.ss -

test:
	gxi -L wikidata.ss -e "(test2)"

run-agent:
	WDQS_UA="$${WDQS_UA:-YourApp/1.0 (https://your.site; you@site)}" gxi -L wikidata.ss -e "(test2-ua)"

test1:
	gxi -L wikidata.ss -e "(test1)"

run-agent1:
	WDQS_UA="$${WDQS_UA:-YourApp/1.0 (https://your.site; you@site)}" gxi -L wikidata.ss -e "(test1-ua)"
  ```

And sample output:

```console
 $ make run-agent1
WDQS_UA="${WDQS_UA:-YourApp/1.0 (https://your.site; you@site)}" gxi -L wikidata.ss -e "(test1-ua)"
Bill Gates URI: http://www.wikidata.org/entity/Q5284
Microsoft URI: http://www.wikidata.org/entity/Q2283
Microsoft->Bill: http://www.wikidata.org/prop/direct/P112
Bill->Microsoft: http://www.wikidata.org/prop/direct/P1830
$ make
gxi -L wikidata.ss -
> (test1)
Bill Gates URI: http://www.wikidata.org/entity/Q5284
Microsoft URI: http://www.wikidata.org/entity/Q2283
Microsoft->Bill: http://www.wikidata.org/prop/direct/P112
Bill->Microsoft: http://www.wikidata.org/prop/direct/P1830
> (test2)
Birth Date: 1862-02-14T00:00:00Z
Birth Place: Venice
```


