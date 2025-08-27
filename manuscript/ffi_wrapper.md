
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


