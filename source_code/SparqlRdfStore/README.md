

Install:

    brew install sord
    brew install rasqal

```
$ make

$ ./DEMO_rdfwrap mini.nt "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/title>	"AI Breakthrough Announced"
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/creator>	<http://example.org/alice>
<http://example.org/alice>	<http://xmlns.com/foaf/0.1/name>	"Alice Smith"

$ ./DEMO_rdfwrap data.ttl "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/title>	"AI Breakthrough Announced"
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/creator>	<http://example.org/alice>
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/date>	"2025-08-27"
<http://example.org/article2>	<http://purl.org/dc/elements/1.1/title>	"Local Team Wins Championship"
<http://example.org/article2>	<http://purl.org/dc/elements/1.1/creator>	<http://example.org/bob>
<http://example.org/alice>	<http://xmlns.com/foaf/0.1/name>	"Alice Smith"
<http://example.org/bob>	<http://xmlns.com/foaf/0.1/name>	"Bob Jones"
```

Gerbil client:

```
# Build the Gerbil executable
$ make TEST_client

# Usage: ./TEST_client [data-file [query]]
# Defaults to data-file=mini.nt and a simple SELECT * pattern
# You can load the query from a file by prefixing with '@' (e.g., @query.sparql)
$ ./TEST_client
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/title>	"AI Breakthrough Announced"
<http://example.org/article1>	<http://purl.org/dc/elements/1.1/creator>	<http://example.org/alice>
<http://example.org/alice>	<http://xmlns.com/foaf/0.1/name>	"Alice Smith"

# Specify a data file and a custom query
$ ./TEST_client data.ttl "SELECT ?s WHERE { ?s ?p ?o } LIMIT 5"
<http://example.org/article1>
<http://example.org/article2>
<http://example.org/alice>
<http://example.org/bob>

# Or read the query from a file
$ cat > q.sparql <<'Q'
SELECT ?s WHERE { ?s ?p ?o } LIMIT 3
Q
$ ./TEST_client data.ttl @q.sparql
<http://example.org/article1>
<http://example.org/article2>
<http://example.org/alice>
```
