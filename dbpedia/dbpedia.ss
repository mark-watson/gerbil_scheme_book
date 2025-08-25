;; File: sparql-client.ss
(import :std/net/request
        :std/text/json
        :std/net/uri) ; For URL encoding
(import :std/format)
(import :std/net/uri)

(export query-dbpedia test2)

;; Helper to process the SPARQL JSON results format
(def (process-sparql-results json-data)
  (let* ((results (hash-ref json-data 'results))
         (bindings (hash-ref results 'bindings)))
    (map (lambda (binding)
           (let ((result-hash (make-hash-table)))
             (hash-for-each
              (lambda (var-name value-obj)
                (hash-put! result-hash
                           (string->symbol var-name)
                           (hash-ref value-obj 'value)))
              binding)
             result-hash))
         bindings)))

(def (query-dbpedia sparql-query)
  (let* ((endpoint "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query={sparql-query}")
         (encoded-query (uri-encode sparql-query))
         (request-url (string-append endpoint "?query=" encoded-query))
         ;; Request the specific JSON format for SPARQL results
         (headers '(("Accept". "application/sparql-results+json"))))

    (let ((response (http-get request-url headers: headers)))
      (if (= (request-status response) 200)
          (let ((response-json (request-json response)))
            (process-sparql-results response-json))
          (error "SPARQL query failed"
                 status: (request-status response)
                 body: (request-text response))))))

;; Example Usage:
(def (test2)
  (let ((query
         "PREFIX dbo: <http://dbpedia.org/ontology/>
          PREFIX dbr: <http://dbpedia.org/resource/>
          SELECT?birthDate?birthPlaceLabel WHERE {
            dbr:Grace_Hopper dbo:birthDate?birthDate.
            dbr:Grace_Hopper dbo:birthPlace?birthPlace.
           ?birthPlace rdfs:label?birthPlaceLabel.
            FILTER (lang(?birthPlaceLabel) = 'en')
          }"))
    (let ((results (query-dbpedia query)))
      (displayln results)
      (for-each
       (lambda (result)
         (display (format "Birth Date: ~a\n" (hash-ref result 'birthDate)))
         (display (format )"Birth Place: ~a\n\n" (hash-ref result 'birthPlaceLabel)))
       results))))