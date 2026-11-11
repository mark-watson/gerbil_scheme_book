;; File: wikidata.ss
(import :std/net/request
        :std/text/json
        :std/net/uri
        :std/format)

(export query-wikidata query-wikidata/alist alist-ref test1 test2 default-user-agent)

;; Default User-Agent following WDQS etiquette policy:
;; https://meta.wikimedia.org/wiki/User-Agent_policy
(def default-user-agent
  (or (getenv "WDQS_UA")
      "GerbilSchemeBookClient/1.0 (https://github.com/mark-watson/gerbil_scheme_book; contact@markwatson.com)"))

;; Convenience: look up a key in an alist. Accepts symbol or string keys.
;; Usage: (alist-ref 'var row [default])
(def (alist-ref key row . default)
  (let* ((sym (if (symbol? key) key (string->symbol key)))
         (p (assq sym row)))
    (if p (cdr p) (if (pair? default) (car default) #f))))

;; Process SPARQL JSON results into a list of hash tables mapping symbol variable names to values.
(def (process-sparql-results json-data)
  (let* ((results (hash-ref json-data 'results))
         (bindings (hash-ref results 'bindings)))
    (map (lambda (binding)
           (let ((result-hash (make-hash-table)))
             (hash-for-each
              (lambda (var-name value-obj)
                (let ((sym (if (symbol? var-name) var-name (string->symbol var-name)))
                      (val (hash-ref value-obj 'value)))
                  (hash-put! result-hash sym val)))
              binding)
             result-hash))
         bindings)))

;; Process SPARQL JSON results into a list of alists: ((var . val) ...)
;; Also preserves datatype and xml:lang attributes when present.
(def (process-sparql-results-alist json-data)
  (let* ((results (hash-ref json-data 'results))
         (bindings (hash-ref results 'bindings)))
    (map (lambda (binding)
           (let ((row '()))
             (hash-for-each
              (lambda (var-name value-obj)
                (let* ((sym (if (symbol? var-name) var-name (string->symbol var-name)))
                       (val (hash-ref value-obj 'value))
                       (dtype (hash-get value-obj 'datatype))
                       (lang (hash-get value-obj 'xml:lang)))
                  (set! row (cons (cons sym val) row))
                  (when dtype
                    (set! row (cons (cons (string->symbol (string-append (symbol->string sym) "-datatype")) dtype) row)))
                  (when lang
                    (set! row (cons (cons (string->symbol (string-append (symbol->string sym) "-lang")) lang) row)))))
              binding)
             (reverse row)))
         bindings)))

;; Query the Wikidata Query Service (WDQS)
;; - Uses GET with URL-encoded query and JSON format
;; - Sends a User-Agent per WDQS guidelines; callers can override via user-agent:
;; - Handles HTTP 429 rate-limiting responses using Retry-After
(def (query-wikidata sparql-query
                    user-agent: (user-agent default-user-agent)
                    as: (as 'hash))
  (let* ((endpoint "https://query.wikidata.org/sparql")
         (encoded-query (uri-encode sparql-query))
         (request-url (string-append endpoint "?query=" encoded-query "&format=json"))
         (headers `(("Accept" . "application/sparql-results+json")
                    ("User-Agent" . ,user-agent))))
    (let loop ((retries 1))
      (let ((response (http-get request-url headers: headers)))
        (cond
         ((= (request-status response) 200)
          (let ((response-json (request-json response)))
            (if (eq? as 'alist)
                (process-sparql-results-alist response-json)
                (process-sparql-results response-json))))
         ((and (= (request-status response) 429) (> retries 0))
          (let* ((headers-alist (request-headers response))
                 (retry-after (or (assoc "retry-after" headers-alist)
                                  (assoc "Retry-After" headers-alist)))
                 (wait-secs (if retry-after (string->number (cdr retry-after)) 5)))
            (thread-sleep! (or wait-secs 5))
            (loop (- retries 1))))
         (else
          (error "SPARQL query failed"
                 status: (request-status response)
                 body: (request-text response))))))))

;; Alist variant returning rows as association lists
(def (query-wikidata/alist sparql-query (user-agent default-user-agent))
  (query-wikidata sparql-query user-agent: user-agent as: 'alist))

;; Test2: fetch birth date and birthplace label for Grace Hopper (wd:Q11641)
(def (test2 (user-agent default-user-agent))
  (let ((query
         (string-append
          "PREFIX wd: <http://www.wikidata.org/entity/>\n"
          "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n"
          "PREFIX wikibase: <http://wikiba.se/ontology#>\n"
          "PREFIX bd: <http://www.bigdata.com/rdf#>\n"
          "SELECT ?birthDate ?birthPlaceLabel WHERE {\n"
          "  wd:Q11641 wdt:P569 ?birthDate .\n"
          "  wd:Q11641 wdt:P19 ?birthPlace .\n"
          "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n"
          "}")))
    (let ((results (query-wikidata/alist query user-agent)))
      (for-each
       (lambda (result)
         (display (format "Birth Date: ~a\n" (alist-ref 'birthDate result)))
         (display (format "Birth Place: ~a\n\n" (alist-ref 'birthPlaceLabel result))))
       results))))

;; Test1: find URIs for Bill Gates and Microsoft; then list relationships
(def (test1 (user-agent default-user-agent))
  (let* ((find-uris
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
         (rows (query-wikidata/alist find-uris user-agent)))
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
               (rels (query-wikidata/alist rel-query user-agent)))
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
