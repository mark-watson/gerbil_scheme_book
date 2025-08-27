
# SparqlRdfStore: a Complete FFI Example Using a Custom C Language Wrapper for Rasqal SPARQL Library and Sord RDF Datastore Library

SparqlRdfStore is a minimal, self-contained bridge between Gerbil Scheme and the well-established C RDF
toolchain. It demonstrates how to load RDF data into an in-memory store and execute SPARQL queries from Gerbil via a tiny Foreign Function Interface (FFI). Under the hood it uses Serd and Sord to parse and hold triples, and Rasqal/Raptor to prepare, execute, and serialize SPARQL results. The project ships with both a small C demo CLI and a Gerbil-based client, making it easy to explore end-to-end from raw data files to query results printed on the console.

The core is a compact C wrapper that exposes three functions: initialize the store with a data file (rdf_init), run a SPARQL query and get results as a string (rdf_query_copy), and clean up (rdf_free). Data is parsed with Serd and stored in Sord’s in-memory model; queries are executed with Rasqal, and results are serialized to TSV by Raptor with the column header removed for line-per-row output. On the Gerbil side, rdfwrap.ss defines the FFI bindings and test.ss provides a simple CLI program, illustrating a clean pattern for calling native libraries from Gerbil without excess machinery.

Using the project is straightforward. After installing the required libraries (Homebrew recipes are listed in the README), make produces a shared library, a C demo (DEMO_rdfwrap), and a Gerbil executable (TEST_client). You can point either binary at a small example dataset (TTL or NT representations are included) and supply a SPARQL SELECT query to print results. The Gerbil client supports sensible defaults, inline queries, and reading queries from files via the @file convention, making it convenient for quick experiments, regression checks, or embedding SPARQL into larger Gerbil programs.

This example application aims to be an educational, practical starting point rather than a full-featured RDF database because data is stred strictly in memory. It focuses on clarity and minimal surface area: a tiny C API, a thin FFI layer, and a simple CLI that you
can adapt. From here, natural extensions include loading multiple graphs, supporting additional RDF syntaxes, handling named graphs/datasets, improving error reporting, and streaming or structuring results beyond TSV. If you’re learning Gerbil FFI, integrating SPARQL into Scheme, or want a small reference for wiring C libraries into a Lisp workflow, SparqlRdfStore provides a concise, working template.

## Library Selection

Here we will be using Sord which is very lightweight C library for storing RDF triples in memory.

Limitation: Sord itself is just a store. It does not have a built-in SPARQL engine. However, it's designed to be used with other libraries, and we will pair it with Rasqal (from the Redland RDF suite) to add SPARQL query capability.

Rasqal is a SPARQL query library. It can parse a SPARQL query and execute it against a graph datastore like Sord or Redland project's librdf. We could have used librdf with Rasqal in our implementation but I felt that it is a better example wrapping libraries from different projects.

## Overview of the Project Structure and Build System

This project contains a sub-directory **C-source** that contains our implementation of a C language wrapper for the Sord and Rasqal libraries. The top level project direcory contains Gerbil Scheme source files, example RDF data files, and a file containing a test SPARQL query:

```bash
$ pwd
~/Users/markw/~GITHUB/gerbil_scheme_book/source_code/SparqlRdfStore
$ ls -R
C-source	data.nt		Makefile	q.sparql	README.md
data.n3		data.ttl	mini.nt		rdfwrap.ss	test.ss

./C-source:
wrapper.c
```

The following Makefile builds the project:

```makefile
# Makefile — DEMO binary + FFI library + Gerbil client

# ---- Prefixes (override on CLI if needed) ----
SORD_PREFIX    ?= /opt/homebrew/opt/sord
SERD_PREFIX    ?= /opt/homebrew/opt/serd
RASQAL_PREFIX  ?= /opt/homebrew/opt/rasqal
RAPTOR_PREFIX  ?= /opt/homebrew/opt/raptor
LIBXML2_PREFIX ?= /opt/homebrew/opt/libxml2
OPENSSL_PREFIX ?= /opt/homebrew/opt/openssl@3

# ---- Tools ----
CC         ?= cc
PKG_CONFIG ?= pkg-config
GXC        ?= gxc

# ---- Sources / Outputs ----
SRC_C      := C-source/wrapper.c
OBJ_C      := $(SRC_C:.c=.o)
OBJ_PIC    := C-source/wrapper.pic.o

DEMO_BIN   := DEMO_rdfwrap
SHLIB      := libRDFWrap.dylib       # macOS
GERBIL_EXE := TEST_client
GERBIL_SRC := test.ss

# ---- Try pkg-config first (brings transitive libs) ----
HAVE_PKGCFG := $(shell $(PKG_CONFIG) --exists sord-0 serd-0 rasqal raptor2 && echo yes || echo no)
PKG_CFLAGS  := $(if $(filter yes,$(HAVE_PKGCFG)),$(shell $(PKG_CONFIG) --cflags sord-0 serd-0 rasqal raptor2))
PKG_LDLIBS  := $(if $(filter yes,$(HAVE_PKGCFG)),$(shell $(PKG_CONFIG) --libs   sord-0 serd-0 rasqal raptor2))

# ---- Fallback include/lib flags ----
FALLBACK_CFLAGS := \
  -I$(SORD_PREFIX)/include/sord-0 \
  -I$(SERD_PREFIX)/include/serd-0 \
  -I$(RASQAL_PREFIX)/include \
  -I$(RAPTOR_PREFIX)/include/raptor2 \
  -I$(LIBXML2_PREFIX)/include/libxml2

# Core libs if pkg-config is unavailable
FALLBACK_LDLIBS := \
  -L$(SORD_PREFIX)/lib   -lsord-0 \
  -L$(SERD_PREFIX)/lib   -lserd-0 \
  -L$(RASQAL_PREFIX)/lib -lrasqal \
  -L$(RAPTOR_PREFIX)/lib -lraptor2 \
  -L$(LIBXML2_PREFIX)/lib -lxml2 \

# Extra libs sometimes needed by transitive deps or gerbil toolchain
EXTRA_LDLIBS := \
  -L$(OPENSSL_PREFIX)/lib -lssl -lcrypto \
  -liconv -lz -lm

# ---- Final flags ----
CFLAGS  ?= -Wall -O2
CFLAGS  += $(if $(PKG_CFLAGS),$(PKG_CFLAGS),$(FALLBACK_CFLAGS))

LDLIBS  += $(if $(PKG_LDLIBS),$(PKG_LDLIBS),$(FALLBACK_LDLIBS)) $(EXTRA_LDLIBS)
LDFLAGS +=

# For the shared lib on macOS
DYNLIB_LDFLAGS := -dynamiclib -install_name @rpath/$(SHLIB)

# Gerbil compile/link flags
GERBIL_CFLAGS := $(CFLAGS)
GERBIL_LDOPTS := $(LDLIBS) -L. -lRDFWrap -Wl,-rpath,@loader_path
# Where gxc writes intermediate artifacts; keep it inside workspace
GERBIL_OUT_DIR ?= .gerbil_build

# ---- Default target ----
all: $(DEMO_BIN) $(SHLIB) $(GERBIL_EXE)

# ---- Demo binary (with small CLI main) ----
$(DEMO_BIN): $(OBJ_C)
	$(CC) -o $@ $^ $(LDFLAGS) $(LDLIBS)

# Build normal object for the demo; define RDF_DEMO_MAIN to enable main()
C-source/wrapper.o: C-source/wrapper.c
	$(CC) $(CFLAGS) -DRDF_DEMO_MAIN -c -o $@ $<

# ---- Shared library for Gerbil FFI ----
$(SHLIB): $(OBJ_PIC)
	$(CC) $(DYNLIB_LDFLAGS) -o $@ $^ $(LDFLAGS) $(LDLIBS)

# PIC object for dynamic library
C-source/wrapper.pic.o: C-source/wrapper.c
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

# ---- Gerbil client (assumes test.ss is valid and calls FFI) ----
$(GERBIL_EXE): $(GERBIL_SRC) rdfwrap.ss $(SHLIB)
	$(GXC) -d $(GERBIL_OUT_DIR) -cc-options "$(GERBIL_CFLAGS)" -ld-options "$(GERBIL_LDOPTS)" -exe -o $@ rdfwrap.ss $(GERBIL_SRC)

# ---- Utilities ----
clean:
	rm -f $(OBJ_C) $(OBJ_PIC) $(DEMO_BIN) $(SHLIB) $(GERBIL_EXE)
	rm -rf $(GERBIL_OUT_DIR)
	rm -rf test DEMO_rdfwrap TEST_client libRDFWrap.dylib .gerbil_build

print-flags:
	@echo "HAVE_PKGCFG = $(HAVE_PKGCFG)"
	@echo "CFLAGS      = $(CFLAGS)"
	@echo "LDFLAGS     = $(LDFLAGS)"
	@echo "LDLIBS      = $(LDLIBS)"
	@echo "GERBIL_CFLAGS = $(GERBIL_CFLAGS)"
	@echo "GERBIL_LDOPTS = $(GERBIL_LDOPTS)"

.PHONY: all clean print-flags
```

TBD

## Implementation of the C Language Wrapper

Here we write a wrapper that will later be called from Gerbil Scheme code.

The following listing shows the wrapper **C-source/wrapper.c**. This C wrapper provides a simplified, high-level interface for handling RDF data by leveraging the combined power of the serd, sord, rasqal, and raptor2 libraries. The primary goal of this wrapper is to abstract away the complexities of library initialization, data parsing, query execution, and results serialization. It exposes a minimal API for loading an RDF graph from a Turtle file and executing SPARQL queries against it, returning the results in a straightforward, easy-to-parse string format. This makes it an ideal solution for applications that need to embed RDF query functionality without managing the intricate details of the underlying RDF processing stack.

```scheme
// C-source/wrapper.c
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <serd/serd.h>
#include <sord/sord.h>
#include <rasqal.h>
#include <raptor2.h>

static SordWorld *g_world = NULL;
static SordModel *g_model = NULL;
static SerdEnv   *g_env   = NULL;
static char      *g_data_path = NULL;

/* ---------- Load RDF into Sord via Serd ---------- */
static int load_turtle_into_sord(const char *path){
  SerdURI base_uri = SERD_URI_NULL;
  SerdNode base = serd_node_new_file_uri((const uint8_t*)path, NULL, &base_uri, true);
  g_env = serd_env_new(&base);
  SerdReader *reader = sord_new_reader(g_model, g_env, SERD_TURTLE, NULL);
  if(!reader){ serd_node_free(&base); return -1; }
  SerdStatus st = serd_reader_read_file(reader, (const uint8_t*)path);
  serd_reader_free(reader);
  serd_node_free(&base);
  return st ? -1 : 0;
}

/* ---------- Public API ---------- */
int rdf_init(const char *n3_path){
  g_world = sord_world_new();
  unsigned idx = SORD_SPO|SORD_OPS|SORD_PSO;
  g_model = sord_new(g_world, idx, false);
  if(load_turtle_into_sord(n3_path)) return -1;
  free(g_data_path);
  g_data_path = strdup(n3_path);
  return 0;
}

char* rdf_query(const char *sparql){
  if(!g_data_path) return NULL;
  rasqal_world *rw = rasqal_new_world(); if(!rw) return NULL;
  if(rasqal_world_open(rw)){ rasqal_free_world(rw); return NULL; }

  rasqal_query *q = rasqal_new_query(rw, "sparql", NULL);
  if(!q){ rasqal_free_world(rw); return NULL; }
  if(rasqal_query_prepare(q, (const unsigned char*)sparql, NULL)){
    rasqal_free_query(q); rasqal_free_world(rw); return NULL;
  }

  raptor_world *rapw = rasqal_world_get_raptor(rw);
  raptor_uri *file_uri = raptor_new_uri_from_uri_or_file_string(rapw, NULL, (const unsigned char*)g_data_path);
  if(!file_uri){ rasqal_free_query(q); rasqal_free_world(rw); return NULL; }

  rasqal_data_graph *dg = rasqal_new_data_graph_from_uri(rw, file_uri, NULL, RASQAL_DATA_GRAPH_BACKGROUND, NULL, NULL, NULL);
  if(!dg){ raptor_free_uri(file_uri); rasqal_free_query(q); rasqal_free_world(rw); return NULL; }
  if(rasqal_query_add_data_graph(q, dg)){
    rasqal_free_data_graph(dg); raptor_free_uri(file_uri);
    rasqal_free_query(q); rasqal_free_world(rw); return NULL;
  }

  rasqal_query_results *res = rasqal_query_execute(q);
  if(!res){ rasqal_free_query(q); rasqal_free_world(rw); return NULL; }

  /* Serialize results as TSV to a malloc'd string */
char *buf = NULL; size_t buflen = 0;
raptor_iostream *ios = raptor_new_iostream_to_string(rapw, (void**)&buf, &buflen, malloc);
if(!ios){ rasqal_free_query_results(res); rasqal_free_query(q); rasqal_free_world(rw); return NULL; }

/* One call handles SELECT/ASK/DESCRIBE/CONSTRUCT */
if(rasqal_query_results_write(ios, res, "tsv", NULL, NULL, NULL) != 0){
  raptor_free_iostream(ios);
  if(buf) free(buf);
  rasqal_free_query_results(res); rasqal_free_query(q); rasqal_free_world(rw);
  return NULL;
}
raptor_free_iostream(ios);

/* Optional: drop TSV header line so output is exactly one line per row */
if(buf){
  char *nl = strchr(buf, '\n');
  if(nl && nl[1]) {
    char *body = strdup(nl+1);
    free(buf);
    buf = body;
  }
}

rasqal_free_query_results(res);
rasqal_free_query(q);
rasqal_free_world(rw);
return buf;
}

void rdf_free(void){
  if(g_env) serd_env_free(g_env);
  if(g_model) sord_free(g_model);
  if(g_world) sord_world_free(g_world);
  g_env=NULL; g_model=NULL; g_world=NULL;
  free(g_data_path); g_data_path=NULL;
}

char* rdf_query_copy(const char *sparql){
  char *s = rdf_query(sparql);
  if (!s) return NULL;
  size_t n = strlen(s);
  char *out = (char*)malloc(n+1);
  if (!out){ free(s); return NULL; }
  memcpy(out, s, n+1);
  free(s);
  return out;
}

/* Optional: tiny demo main if you want a CLI.
   Compile by adding -DRDF_DEMO_MAIN to CFLAGS, then:
   ./rdfwrap news.n3 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 3'
*/
#ifdef RDF_DEMO_MAIN
int main(int argc, char** argv){
  if(argc < 3){ fprintf(stderr,"usage: %s <data.ttl> <sparql>\n", argv[0]); return 1; }
  if(rdf_init(argv[1])){ fprintf(stderr,"failed to load %s\n", argv[1]); return 2; }
  char* s = rdf_query(argv[2]);
  if(!s){ fprintf(stderr,"query failed\n"); rdf_free(); return 3; }
  fputs(s, stdout); free(s);
  rdf_free(); return 0;
}
#endif
```

The core functionality begins with initialization and data loading, handled by the rdf_init() function. This function sets up the necessary in-memory storage by creating a SordWorld and a SordModel, which serve as the container for the RDF graph. It then calls the internal helper function load_turtle_into_sord(), which utilizes the serd parser to efficiently read and load the triples from a specified Turtle (.ttl) file into the sord model. This process establishes the in-memory database that all subsequent queries will be executed against. The path to the data file is stored globally for later use by the query engine.

Once the data is loaded, the rdf_query() function provides the mechanism for executing SPARQL queries. It orchestrates the rasqal query engine to parse and prepare the SPARQL query string. The function then uses the raptor2 library to create a URI reference to the original data file and associates it with the query as a data graph. After executing the query, rasqal and raptor work together to serialize the query results into a Tab-Separated Values (TSV) formatted string. As a final convenience, the code processes this string to remove the header row, ensuring that the returned buffer contains only the result data, with each row representing a solution.

Finally, the wrapper provides essential memory management and utility functions. The rdf_free() function is responsible for cleanly deallocating all resources, including the sord world and model, the serd environment, and any other globally allocated memory, preventing memory leaks. The rdf_query_copy() function is a simple convenience utility that executes a query via rdf_query() and returns a new, separately allocated copy of the result string, which can be useful for certain memory management patterns. The code also includes an optional main function, enabled by the RDF_DEMO_MAIN macro, which demonstrates the wrapper's usage and allows it to function as a standalone command-line tool for quick testing.

## A Gerbil Scheme Shim to Call The C Language Wrapper Code

The shim code is n the source file **rdfwrap.ss**:

```scheme
;; rdfwrap.ss — minimal FFI for libRDFWrap.dylib using :std/foreign
(import :std/foreign)
(export rdf-init rdf-query rdf-free)

;; Wrap FFI forms in begin-ffi so helper macros are available and exports are set
(begin-ffi (rdf-init rdf-query rdf-free)
  ;; Declare the C functions provided by libRDFWrap.dylib
  (c-declare "
    #include <stdlib.h>
    int   rdf_init(const char*);
    char* rdf_query_copy(const char*);
    void  rdf_free(void);
  ")

  ;; FFI bindings
  (define-c-lambda rdf-init  (char-string) int         "rdf_init")
  (define-c-lambda rdf-query (char-string) char-string "rdf_query_copy")
  (define-c-lambda rdf-free  ()            void        "rdf_free"))
```

This Scheme source file serves as a crucial Foreign Function Interface (FFI) shim, creating a bridge between the high-level Scheme environment and the low-level C functions compiled into the libRDFWrap.dylib shared library. Its purpose is to "wrap" the C functions, making them directly callable from Scheme code as if they were native procedures. By handling the data type conversions and function call mechanics, this shim abstracts away the complexity of interoperating between the two languages, providing a clean and idiomatic Scheme API for the underlying RDF processing engine.

The entire FFI definition is encapsulated within a (begin-ffi ...) block, which sets up the necessary context for interfacing with foreign code. Inside this block, the first step is the c-declare form. This form contains a string of C code that declares the function prototypes for the C library's public API. By providing the signatures for rdf_init, rdf_query_copy, and rdf_free, it informs the Scheme FFI about the exact names, argument types, and return types of the C functions it needs to connect with. This declaration acts as a contract, ensuring that the Scheme bindings will match the expectations of the compiled C library.

Following the declaration, the script defines the actual Scheme procedures using define-c-lambda. Each of these forms creates a direct binding from a new Scheme function to a C function. For instance, (define-c-lambda rdf-init ...) creates a Scheme function named rdf-init that calls the C function of the same name. This form also specifies the marshalling of data types between the two environments, such as converting a Scheme string to a C char-string (const char*).

Notably, the Scheme function rdf-query is explicitly mapped to the C function rdf_query_copy. This is a deliberate design choice to simplify memory management. The rdf_query_copy function in C returns a distinct, newly allocated copy of the result string. This prevents the Scheme garbage collector from trying to manage memory that was allocated by the C library's malloc, avoiding potential conflicts and memory corruption. The final binding for rdf-free provides a way to call the C cleanup function, ensuring that all resources allocated by the C library are properly released.

## Main Program for the SparqlRdfStore Example

The main test program is in the file **test.ss**:

```scheme
(import "rdfwrap" :std/srfi/13)
(export main)

(define default-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }")

(define (usage name)
  (display "Usage: ") (display name) (display " [data-file [query]]\n")
  (display "  data-file: RDF file (ttl/n3/nt). Default: mini.nt\n")
  (display "  query    : SPARQL SELECT query or @file to read from file. Default: ")
  (display default-query) (newline))

(define (main . args)
  (let* ((prog (car (command-line)))
         (path (if (pair? args) (car args) "mini.nt"))
         (rawq (if (and (pair? args) (pair? (cdr args))) (cadr args) default-query))
         (query (if (string-prefix? "@" rawq)
                     (let ((f (substring rawq 1 (string-length rawq))))
                       (call-with-input-file f
                         (lambda (p)
                           (let loop ((acc '()))
                             (let ((ch (read-char p)))
                               (if (eof-object? ch)
                                   (list->string (reverse acc))
                                   (loop (cons ch acc))))))))
                     rawq)))
    (when (or (string=? path "-h") (string=? path "--help"))
      (usage prog)
      (exit 0))
    (unless (zero? (rdf-init path)) (error "rdf-init failed" path))
    (let ((out (rdf-query query)))
      (when out (display out)))
    (rdf-free)
    (exit 0)))
```

This Scheme script provides a user-friendly command-line interface (CLI) for the underlying C-based RDF wrapper. It acts as a high-level controller, responsible for parsing user input, managing file operations, and invoking the core RDF processing functions exposed by the C library. The script allows a user to specify an RDF data file and a SPARQL query directly on the command line or from a file. By handling argument parsing and I/O, it simplifies the process of interacting with the RDF engine, making it accessible and easy to use for quick queries and testing without needing to write and compile C code.

The script's main function serves as the primary entry point and is responsible for processing command-line arguments. It intelligently determines the RDF data file path and the SPARQL query string from the arguments provided by the user. If the user omits these arguments, the script falls back to sensible defaults: "mini.nt" for the data file and a simple SELECT query to fetch all triples. Furthermore, it includes a basic help mechanism, displaying a usage message if the user provides "-h" or "--help" as an argument, guiding them on the correct command structure.

A key feature of the script is its ability to read the SPARQL query from two different sources. By default, it treats the command-line argument as the query string itself. However, if the query argument is prefixed with an "@" symbol (e.g., @myquery.sparql), the script interprets the rest of the string as a filename. It then proceeds to open and read the entire contents of that file, loading it into memory as the query to be executed. This flexibility allows users to easily run complex, multi-line queries that would be cumbersome to type directly into the terminal.

After parsing the inputs, the script interfaces directly with the C wrapper's functions. It first calls rdf-init to load the specified RDF data file into the in-memory model. If initialization is successful, it passes the prepared SPARQL query to the rdf-query function, which executes the query and returns the results as a single string. The script then prints this result string to standard output. Finally, to ensure proper resource management and prevent memory leaks, it calls rdf-free to clean up the C library's allocated resources before the program terminates.

