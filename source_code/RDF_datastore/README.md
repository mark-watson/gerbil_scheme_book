# RDF Datastore — Simple In-Memory RDF Store with Toy SPARQL

**Book Chapter:** [A Simple In-Memory RDF Store and Query Language in Gerbil Scheme](https://leanpub.com/read/Gerbil-Scheme/a-simple-in-memory-rdf-store-and-query-language-in-gerbil-scheme) — *Gerbil Scheme in Action* (free to read online).


A pure-Gerbil Scheme implementation of an in-memory RDF triple store with
support for a useful subset of SPARQL `SELECT` queries.  No external
libraries or C FFI are required.

## Files

| File | Description |
|------|-------------|
| `RDF.ss` | Triple store implementation + runnable demo |
| `gerbil.pkg` | Gerbil package declaration (`rdf-datastore`) |
| `Makefile` | `run`, `compile`, and `clean` targets |

## Quick Start

```sh
make run        # interpret and run the demo with gxi
```

Or directly:

```sh
gxi RDF.ss
```

## Makefile Targets

| Target | Description |
|--------|-------------|
| `make run` | Run the demo via the Gerbil interpreter (`gxi`) — no compilation needed |
| `make compile` | Compile with `gxc`; module becomes importable as `:rdf-datastore/RDF` |
| `make clean` | Remove `.gerbil/` and compiled artefacts (`*.o*`, `*.ssxi`, `*.ssi`) |

## Architecture

![Generated image](architecture.png)

## API

### Store management

```scheme
(make-store)                   ; → store   — create an empty triple store
(add-triple    st s p o)       ; → store   — add triple (s p o), returns store
(remove-triple st s p o)       ; → store   — remove matching triple, returns store
(store-triples st)             ; → list    — all triples as (s p o) lists
(print-all-triples st)         ;           — display all triples to stdout
```

### Querying

```scheme
(sparql-select st query-string)  ; → list of alists
```

Each result row is an association list mapping variable name strings to
their bound values, e.g. `(("?name" . "\"Alice\"") ("?age" . "30"))`.

## SPARQL Subset

```sparql
SELECT ?var1 ?var2 ... WHERE { s p o . s p o . }
SELECT * WHERE { ?s ?p ?o . }
```

- Variables start with `?`
- Multiple triple patterns are joined (inner join, nested-loop)
- `SELECT *` returns the full binding environment for every solution
- Trailing `.` between patterns is optional

## Example Session

```scheme
(define st (make-store))

(add-triple st "<ex:alice>" "<foaf:name>"  "\"Alice\"")
(add-triple st "<ex:alice>" "<foaf:knows>" "<ex:bob>")
(add-triple st "<ex:bob>"   "<foaf:name>"  "\"Bob\"")

(sparql-select st
  "SELECT ?friendName WHERE {
     <ex:alice> <foaf:knows> ?friend .
     ?friend    <foaf:name>  ?friendName .
   }")
;; → (( ("?friendname" . "\"Bob\"") ))
```

## Demo Output

Running `make run` produces:

```
All triples:
  <ex:carol> <foaf:age> 35
  <ex:carol> <foaf:name> "Carol"
  <ex:bob>   <foaf:age> 25
  <ex:bob>   <foaf:name> "Bob"
  <ex:alice> <foaf:age> 30
  <ex:alice> <foaf:knows> <ex:bob>
  <ex:alice> <foaf:name> "Alice"

Query: Who does Alice know?
  ?friendname: "Bob"

Query: All names
  ?s: <ex:carol>  ?name: "Carol"
  ?s: <ex:bob>    ?name: "Bob"
  ?s: <ex:alice>  ?name: "Alice"

Query: All ages
  ?person: <ex:carol>  ?age: 35
  ?person: <ex:bob>    ?age: 25
  ?person: <ex:alice>  ?age: 30

Query: SELECT * (all triples)
  ... (all 7 triples as full bindings) ...

After removing <ex:bob> <foaf:age> 25:
Query: All ages after removal
  ?person: <ex:carol>  ?age: 35
  ?person: <ex:alice>  ?age: 30
```

## Implementation Notes

- **Store** — a mutable cons-cell wrapping a list of `(s p o)` triples.
  Both `add-triple` and `remove-triple` mutate in place and return the store
  so calls can be chained.
- **Tokenizer** — walks the query character-by-character, splitting on
  whitespace and punctuation (`{ } . , ; ( )`).
- **Parser** — uses `let-values` to return two values: the selected variable
  list and the list of `(s p o)` patterns. Handles `SELECT *` as a wildcard.
- **Evaluator** — classic nested-loop join (`join-patterns`): for each
  pattern, extends every surviving binding environment by matching against
  all triples.
- **Environments** — association lists `((var . val) …)`.
  `env-extend` detects conflicts and returns `#f`, which short-circuits
  failed matches cleanly.
