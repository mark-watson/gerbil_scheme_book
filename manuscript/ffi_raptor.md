# Gerbil Scheme FFI Example Using the C Language Raptor RDF Library

The Foreign Function Interface (FFI) in Gerbil Scheme provides a powerful bridge to leverage existing C libraries directly within Scheme programs. This allows developers to extend Scheme applications with highly optimized, domain-specific functionality written in C, while still enjoying the high-level abstractions and rapid development style of Scheme. In this chapter, we will explore an end-to-end example of integrating the [Raptor RDF parsing and serialization library](https://librdf.org/raptor/) into Gerbil Scheme, showing how to bind C functions and expose them as Scheme procedures.

Raptor is a mature C library for parsing, serializing, and manipulating RDF (Resource Description Framework) data in a variety of syntaxes, including RDF/XML, Turtle, N-Triples, and JSON-LD. By accessing Raptor from Gerbil Scheme, we open the door to semantic web applications, linked data processing, and graph-based reasoning directly within a Scheme environment. This example illustrates the mechanics of building FFI bindings, handling C-level memory and data types, and translating them into idiomatic Scheme representations. For reference, the official Raptor API documentation is available here: Raptor2 API Reference.

We will walk through the process step by step: setting up the Gerbil Scheme FFI definitions, mapping C functions and structs into Scheme, and writing test programs that parse RDF data and extract triples. Along the way, we will highlight practical issues such as error handling, memory management, symbol exporting, and resource cleanup. By the end of this chapter, you should have both a working Gerbil Scheme binding to Raptor and a general blueprint for integrating other C libraries into your Scheme projects. For background on Gerbil Scheme's FFI itself, consult the [Gerbil Documentation: FFI](https://gerbil.scheme.org/guide/ffi.html).

## Implementation of a FFI Bridge Library for Raptor

The library is located in the file **gerbil_scheme_book/source_code/RaptorRDF_FFI/ffi.ss**. This code demonstrates how to use the Foreign Function Interface (FFI) to integrate with the Raptor RDF C library. It provides three levels of Scheme-accessible procedures: a low-level **raptor-parse-file->ntriples** that returns raw N-Triples text, a high-level **parse-rdf-file** with error handling, and a structured **parse-rdf-file->triples** that returns parsed Scheme lists. This example highlights the practical use of FFI in Gerbil Scheme: exposing a C function to Scheme, managing memory safely across the boundary, and translating RDF data into representations that Scheme programs can process directly.

### The C FFI Layer

The first part of **ffi.ss** embeds C code inline using Gerbil's **begin-ffi** / **c-declare** mechanism:

```scheme
(export raptor-parse-file->ntriples   ;; low-level (returns #f on error)
        parse-rdf-file                ;; high-level (raises on error)
        parse-rdf-file->triples)      ;; structured (list of s/p/o lists)

(import :std/foreign)

(begin-ffi (raptor-parse-file->ntriples)
  (c-declare #<<'C'
#include <raptor2.h>
#include <string.h>
#include <stdlib.h>

#ifndef RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES
#define RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES 0x4
#endif

/* Write one triple to the iostream in N-Triples and a newline */
static void triples_to_iostr(void* user_data, raptor_statement* st) {
  raptor_iostream* iostr = (raptor_iostream*)user_data;

  raptor_term_escaped_write(st->subject, RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES, iostr);
  raptor_iostream_write_byte(' ', iostr);
  raptor_term_escaped_write(st->predicate, RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES, iostr);
  raptor_iostream_write_byte(' ', iostr);
  raptor_term_escaped_write(st->object, RAPTOR_STRING_ESCAPE_FLAG_NTRIPLES, iostr);
  raptor_iostream_write_byte(' ', iostr);
  raptor_iostream_write_byte('.', iostr);
  raptor_iostream_write_byte('\n', iostr);
}

/* Parse `filename` with syntax `syntax_name` and return N-Triples as char*.
   The caller receives a standard malloc'd copy; the Raptor buffer is freed
   here to avoid leaking memory. */
static char* parse_file_to_ntriples(const char* filename, const char* syntax_name) {
  raptor_world *world = NULL;
  raptor_parser* parser = NULL;
  unsigned char *uri_str = NULL;
  raptor_uri *uri = NULL, *base_uri = NULL;
  raptor_iostream *iostr = NULL;
  void *out_string = NULL;
  size_t out_len = 0;
  char *result = NULL;

  world = raptor_new_world();
  if(!world) return NULL;
  if(raptor_world_open(world)) { raptor_free_world(world); return NULL; }

  /* Where triples go: a string iostream that materializes on free */
  iostr = raptor_new_iostream_to_string(world, &out_string, &out_len, NULL);
  if(!iostr) { raptor_free_world(world); return NULL; }

  parser = raptor_new_parser(world, syntax_name ? syntax_name : "guess");
  if(!parser) { raptor_free_iostream(iostr); raptor_free_world(world); return NULL; }

  raptor_parser_set_statement_handler(parser, iostr, triples_to_iostr);

  uri_str = raptor_uri_filename_to_uri_string((const unsigned char*)filename);
  if(!uri_str) { raptor_free_parser(parser); raptor_free_iostream(iostr); raptor_free_world(world); return NULL; }

  uri = raptor_new_uri(world, uri_str);
  base_uri = raptor_uri_copy(uri);

  /* Parse file; on each triple our handler appends to iostr */
  raptor_parser_parse_file(parser, uri, base_uri);

  /* Clean up parser/URIs; free iostr LAST to finalize string */
  raptor_free_parser(parser);
  raptor_free_uri(base_uri);
  raptor_free_uri(uri);
  raptor_free_memory(uri_str);

  raptor_free_iostream(iostr); /* this finalizes out_string/out_len */

  /* Copy the raptor-allocated string into standard malloc'd memory,
     then free the raptor buffer to avoid a memory leak. */
  if (out_string) {
    result = strdup((char*)out_string);
    raptor_free_memory(out_string);
  }

  raptor_free_world(world);

  return result;  /* Gambit copies to Scheme string via char-string */
}
'C'
  )

  ;; Scheme visible wrapper:
  (define-c-lambda raptor-parse-file->ntriples
    (char-string       ;; filename
     char-string)      ;; syntax name, e.g., "turtle", "rdfxml", or "guess"
    char-string
    "parse_file_to_ntriples"))
```

The C portion begins by including the **raptor2.h** header and defining a callback function, **triples_to_iostr**, which takes RDF statements and writes them to a Raptor **iostream** in N-Triples format. This callback escapes subjects, predicates, and objects correctly and ensures triples are terminated with a period and newline, conforming to the N-Triples standard. The main work is performed in **parse_file_to_ntriples**, which initializes a Raptor world and parser, configures the statement handler to use the callback, and sets up an **iostream** that accumulates parsed triples into a string buffer. Error checks are in place at every step, ensuring resources such as the world, parser, URIs, and iostream are properly freed if initialization fails.

After setup, the parser processes the input file identified by its filename and syntax. Each RDF statement is converted into N-Triples and appended to the output string via the **iostream**. Once parsing is complete, the parser, URIs, iostream, and world are released.

An important detail in the memory management: Raptor's **raptor_new_iostream_to_string** allocates its output buffer using an internal allocator, so the caller is responsible for freeing it with **raptor_free_memory**. The C function uses **strdup** to copy the Raptor-allocated string into standard **malloc**'d memory, then immediately frees the original with **raptor_free_memory**. This prevents a memory leak that would otherwise occur on every call. The copy is then returned to Gambit, which makes its own GC-managed copy into a Scheme string via the **char-string** return type convention.

On the Scheme side, the **define-c-lambda** form binds this C function as the procedure **raptor-parse-file->ntriples**, exposing it with the expected **(filename syntax-name) -> ntriples-string** interface.

The following architecture diagram shows the structure of this FFI example, illustrating how Gerbil Scheme calls through the FFI boundary into the C wrapper code, which in turn uses the Raptor2 library to parse RDF files and serialize triples into N-Triples format.

{width: "80%"}
![Architecture diagram for the Raptor RDF FFI example](ffi_raptor_architecture.png)

### High-Level Scheme API

Below the FFI layer, **ffi.ss** provides several pure Scheme functions that make the library easier to use. These use only standard Gambit/Gerbil builtins and require no additional imports.

First, two small utility functions: **char-find** locates a character in a string starting from a given position, and **split-lines** splits a string on newline characters:

```scheme
;; Find the position of the first occurrence of char CH in STR
;; starting at FROM.  Returns #f if not found.
(def (char-find str ch (from 0))
  (let ((len (string-length str)))
    (let loop ((i from))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref str i) ch) i)
        (else (loop (+ i 1)))))))

;; Split STR on newlines, returning a list of strings.
(def (split-lines str)
  (let ((len (string-length str)))
    (let loop ((start 0) (acc '()))
      (let ((pos (char-find str #\newline start)))
        (if pos
            (loop (+ pos 1) (cons (substring str start pos) acc))
            (reverse (cons (substring str start len) acc)))))))
```

The first convenience wrapper is **parse-rdf-file**, which adds a default syntax of "guess" and raises a clear error instead of returning a silent empty string when parsing fails:

```scheme
;; Parse an RDF file and return N-Triples as a string.
;; SYNTAX can be "turtle", "rdfxml", "ntriples", or "guess" (default).
;; Signals an error if parsing fails.
(def (parse-rdf-file filename (syntax "guess"))
  (let ((result (raptor-parse-file->ntriples filename syntax)))
    (if result
        result
        (error "Failed to parse RDF file" filename syntax))))
```

For working with the N-Triples output as structured data, three more helper functions parse individual lines and assemble them into Scheme lists. **strip-trailing-dot** removes the trailing " ." from N-Triples object fields, **skip-spaces** finds the first non-space character in a line, and **parse-ntriple-line** combines them to extract subject, predicate, and object from a single line:

```scheme
;; Strip trailing " ." from an N-Triples object field.
(def (strip-trailing-dot str)
  (let ((len (string-length str)))
    (if (and (>= len 2)
             (char=? (string-ref str (- len 1)) #\.)
             (char=? (string-ref str (- len 2)) #\space))
        (substring str 0 (- len 2))
        str)))

;; Find the start of non-space content in LINE.  Returns #f if blank.
(def (skip-spaces line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref line i) #\space) (loop (+ i 1)))
        (else i)))))

;; Parse a single N-Triples line into (subject predicate object), or #f.
;; Skips blank lines and comments.
(def (parse-ntriple-line line)
  (let ((start (skip-spaces line)))
    (cond
      ((not start) #f)
      ((char=? (string-ref line start) #\#) #f)
      (else
       (let ((s-end (char-find line #\space start)))
         (and s-end
              (let ((p-end (char-find line #\space (+ s-end 1))))
                (and p-end
                     (let ((subj (substring line start s-end))
                           (pred (substring line (+ s-end 1) p-end))
                           (obj  (strip-trailing-dot
                                  (substring line (+ p-end 1) (string-length line)))))
                       (list subj pred obj))))))))))
```

Finally, **parse-rdf-file->triples** ties everything together: it parses a file, splits the resulting N-Triples into lines, and returns a list of **(subject predicate object)** triples as Scheme lists:

```scheme
;; Parse an RDF file and return a list of (subject predicate object) lists.
(def (parse-rdf-file->triples filename (syntax "guess"))
  (let ((nt-string (parse-rdf-file filename syntax)))
    (let loop ((lines (split-lines nt-string)) (acc '()))
      (if (null? lines)
          (reverse acc)
          (let ((parsed (parse-ntriple-line (car lines))))
            (if parsed
                (loop (cdr lines) (cons parsed acc))
                (loop (cdr lines) acc)))))))
```

This layered design gives readers three entry points depending on their needs: raw N-Triples text from the low-level FFI call, a safe string-returning wrapper with error handling, or fully structured Scheme data ready for further processing.

## Test Code

This Gerbil Scheme test code in the file **test.ss** exercises all three API levels by creating a minimal Turtle input, invoking the parser in multiple modes, testing the structured output, and verifying error behavior on nonexistent files. It uses **call-with-output-file** (a Gambit builtin) to write the temporary test file:

```scheme
;; Test suite for ffi.ss: validates N-Triples output from Raptor2 FFI

(import "ffi" :gerbil/gambit)
(export main)

(define (assert-equal expected actual label)
  (if (equal? expected actual)
      (begin (display "PASS ") (display label) (newline) #t)
      (begin
        (display "FAIL ") (display label) (newline)
        (display "Expected:\n") (display expected) (newline)
        (display "Actual:\n") (display actual) (newline)
        (exit 1))))

(define (main . args)
  (let* ((ttl-file "sample.ttl")
         (ttl-content "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .\n")
         (expected-nt "<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n"))

    ;; Prepare sample Turtle file
    (call-with-output-file ttl-file
      (lambda (p) (display ttl-content p)))

    ;; 1. Parse with explicit syntax
    (let ((nt1 (raptor-parse-file->ntriples ttl-file "turtle")))
      (assert-equal expected-nt nt1 "turtle -> ntriples"))

    ;; 2. Parse with syntax guessing
    (let ((nt2 (raptor-parse-file->ntriples ttl-file "guess")))
      (assert-equal expected-nt nt2 "guess -> ntriples"))

    ;; 3. High-level API with default syntax
    (let ((nt3 (parse-rdf-file ttl-file)))
      (assert-equal expected-nt nt3 "parse-rdf-file (default syntax)"))

    ;; 4. Structured triples API
    (let ((triples (parse-rdf-file->triples ttl-file)))
      (assert-equal 1 (length triples) "parse-rdf-file->triples returns 1 triple")
      (assert-equal '("<http://example.org/s>"
                      "<http://example.org/p>"
                      "<http://example.org/o>")
                    (car triples)
                    "parse-rdf-file->triples structure"))

    ;; 5. Error handling: nonexistent file returns empty string
    (let ((bad (raptor-parse-file->ntriples "no-such-file.ttl" "turtle")))
      (assert-equal "" bad "nonexistent file returns empty string"))

    ;; Clean up
    (when (file-exists? ttl-file)
      (delete-file ttl-file))

    (display "All tests passed.\n")))
```

The test imports the FFI wrapper and exports a **main** entry point. The **assert-equal** helper prints PASS/FAIL labels and exits with non-zero status on mismatch. In function **main**, a small Turtle document defines a simple triple using the ex: prefix; the corresponding expected N-Triples string is the fully expanded IRI form with a terminating period and newline.

The test proceeds in five phases: first it calls **raptor-parse-file->ntriples** with explicit "turtle" syntax; then repeats using "guess" to confirm auto-detection yields identical output; third, it tests the high-level **parse-rdf-file** wrapper with its default syntax; fourth, it exercises **parse-rdf-file->triples** and verifies that the returned Scheme list contains the correct subject, predicate, and object strings; finally, it confirms that attempting to parse a nonexistent file produces an empty string rather than crashing. After all assertions pass, the temporary file is deleted.

## Build System

The Makefile uses **pkg-config** to auto-detect Raptor2's include and library paths, with a Homebrew fallback for macOS systems where pkg-config may not be in the default search path. This single Makefile works on both macOS and Linux:

```makefile
##### macOS and Linux — auto-detects raptor2 paths
#
# Requires: raptor2, pkg-config
#   macOS:  brew install raptor pkg-config
#   Linux:  sudo apt install libraptor2-dev pkg-config

# Try pkg-config first; fall back to brew --prefix on macOS
RAPTOR_PKG := $(shell pkg-config --exists raptor2 2>/dev/null && echo yes)
ifeq ($(RAPTOR_PKG),yes)
  CC_OPTS := $(shell pkg-config --cflags raptor2)
  LD_OPTS := $(shell pkg-config --libs raptor2)
else
  # Homebrew keg-only fallback (macOS)
  RAPTOR_PREFIX := $(shell brew --prefix raptor 2>/dev/null)
  ifdef RAPTOR_PREFIX
    CC_OPTS := -I$(RAPTOR_PREFIX)/include/raptor2
    LD_OPTS := -L$(RAPTOR_PREFIX)/lib -lraptor2
  else
    $(error raptor2 not found. Install with: brew install raptor  OR  sudo apt install libraptor2-dev)
  endif
endif

build:
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" ffi.ss
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" -exe -o test test.ss

clean:
	rm -f *.c *.scm *.o *.so test
```

The Makefile first checks whether **pkg-config** knows about **raptor2**. If so, it uses the standard **--cflags** and **--libs** flags. Otherwise, on macOS, it falls back to **brew --prefix raptor** to locate the Homebrew installation. If neither method finds Raptor, the build stops with a helpful error message showing the install command for both platforms.

Build and test output:

```
$ make
gxc -cc-options "..." -ld-options "..." ffi.ss
gxc -cc-options "..." -ld-options "..." -exe -o test test.ss
$ ./test
PASS turtle -> ntriples
PASS guess -> ntriples
PASS parse-rdf-file (default syntax)
PASS parse-rdf-file->triples returns 1 triple
PASS parse-rdf-file->triples structure
PASS nonexistent file returns empty string
All tests passed.
```

## Optional Practice Problems

1. **In-Memory Parsing**: Modify the C and Scheme FFI logic in `ffi.ss` to add support for parsing RDF from an in-memory string buffer rather than reading it from a file path on disk.
2. **DataType and LangTag Handling**: The current `parse-ntriple-line` splits on spaces. Extend it to properly handle and parse literals with language tags (like `"apple"@en`) or XML Schema datatypes (like `"123"^^<http://www.w3.org/2001/XMLSchema#integer>`).
3. **RDF Query Helper**: Write a Scheme function using `parse-rdf-file->triples` that allows users to query and filter the resulting triples list by a specific subject or predicate URI.

