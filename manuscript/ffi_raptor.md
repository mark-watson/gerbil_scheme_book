# Gerbil Scheme FFI Example Using the C Language Raptor RDF Library

The Foreign Function Interface (FFI) in Gerbil Scheme provides a powerful bridge to leverage existing C libraries directly within Scheme programs. This allows developers to extend Scheme applications with highly optimized, domain-specific functionality written in C, while still enjoying the high-level abstractions and rapid development style of Scheme. In this chapter, we will explore an end-to-end example of integrating the [Raptor RDF parsing and serialization library](https://librdf.org/raptor/) into Gerbil Scheme, showing how to bind C functions and expose them as Scheme procedures.

Raptor is a mature C library for parsing, serializing, and manipulating RDF (Resource Description Framework) data in a variety of syntaxes, including RDF/XML, Turtle, N-Triples, and JSON-LD. By accessing Raptor from Gerbil Scheme, we open the door to semantic web applications, linked data processing, and graph-based reasoning directly within a Scheme environment. This example illustrates the mechanics of building FFI bindings, handling C-level memory and data types, and translating them into idiomatic Scheme representations. For reference, the official Raptor API documentation is available here: Raptor2 API Reference.

We will walk through the process step by step: setting up the Gerbil Scheme FFI definitions, mapping C functions and structs into Scheme, and writing test programs that parse RDF data and extract triples. Along the way, we will highlight practical issues such as error handling, symbol exporting, and resource cleanup. By the end of this chapter, you should have both a working Gerbil Scheme binding to Raptor and a general blueprint for integrating other C libraries into your Scheme projects. For background on Gerbil Scheme’s FFI itself, consult the [Gerbil Documentation: FFI](https://gerbil.scheme.org/guide/ffi.html).

## Implementation of a FFI Bridge Library for Raptor

The library is located in the file ** gerbil_scheme_book/source_code/RaptorRDF_FFI/ffi.ss**. This code demonstrates how to use the Foreign Function Interface (FFI) to integrate with the Raptor RDF C library. It provides a Scheme-accessible procedure, **raptor-parse-file->ntriples**, which parses an RDF file in a specified syntax (such as Turtle or RDF/XML) and returns the results as an N-Triples–formatted string. This example highlights the practical use of FFI in Gerbil Scheme: exposing a C function to Scheme, managing memory safely across the boundary, and translating RDF data into a representation that Scheme programs can process directly.

```scheme
(export raptor-parse-file->ntriples)

(import :std/foreign)

(begin-ffi (raptor-parse-file->ntriples)
  (c-declare #<<'C'
#include <raptor2.h>
#include <string.h>

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
   The returned memory is owned by Raptor's allocator; Gambit copies it
   into a Scheme string via char-string return convention. */
static char* parse_file_to_ntriples(const char* filename, const char* syntax_name) {
  raptor_world *world = NULL;
  raptor_parser* parser = NULL;
  unsigned char *uri_str = NULL;
  raptor_uri *uri = NULL, *base_uri = NULL;
  raptor_iostream *iostr = NULL;
  void *out_string = NULL;
  size_t out_len = 0;

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

  /* Keep world only as long as needed; string is independent now */
  raptor_free_world(world);

  return (char*)out_string; /* Gambit copies to Scheme string */
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

The C portion begins by including the raptor2.h header and defining a callback function, **triples_to_iostr**, which takes RDF statements and writes them to a Raptor **iostream** in N-Triples format. This callback escapes subjects, predicates, and objects correctly and ensures triples are terminated with a period and newline, conforming to the N-Triples standard. The main work is performed in **parse_file_to_ntriples**, which initializes a Raptor world and parser, configures the statement handler to use the callback, and sets up an **iostream** that accumulates parsed triples into a string buffer. Error checks are in place at every step, ensuring resources such as the world, parser, URIs, and iostream are properly freed if initialization fails.

After setup, the parser processes the input file identified by its filename and syntax. Each RDF statement is converted into N-Triples and appended to the output string via the **iostream**. Once parsing is complete, the parser, URIs, iostream, and world are released, leaving a fully materialized string containing the N-Triples serialization. This string is returned to Scheme through the FFI, where Gambit copies it into a managed Scheme string. On the Scheme side, the **define-c-lambda** form binds this C function as the procedure **raptor-parse-file->ntriples**, exposing it with the expected **(filename syntax-name) -> ntriples-string** interface. The result is a clean abstraction: Scheme code can call **raptor-parse-file->ntriples** with an RDF file and syntax, receiving back normalized N-Triples ready for further processing in Gerbil Scheme.

## Test Code

This Gerbil Scheme testcode in the file **test.ss** exercises the FFI binding **raptor-parse-file->ntriples** by creating a minimal Turtle input, invoking the parser in two modes (“turtle” and “guess”), and asserting that both produce the same canonical N-Triples output. It’s designed to be self-contained: it writes a temporary .ttl file, runs the conversion twice, compares results against an expected string, then cleans up and prints status.

```scheme
;; Simple test for ffi.ss: validates N-Triples output

(import "ffi")
(export main)
(import "ffi" :gerbil/gambit)

(define (write-file path content)
  (let ((p (open-output-file path)))
    (display content p)
    (close-output-port p)))

(define (read-file path)
  (let ((p (open-input-file path)))
    (let loop ((chunks '()))
      (let ((c (read-char p)))
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse chunks)))
            (loop (cons c chunks)))))))

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
         (ttl-content "@prefix ex: <http://example.org/> .
ex:s ex:p ex:o .
")
         (expected-nt "<http://example.org/s> <http://example.org/p> <http://example.org/o> .
"))
    ;; Prepare sample Turtle file
    (write-file ttl-file ttl-content)

    ;; Exercise FFI with explicit syntax
    (let ((nt1 (raptor-parse-file->ntriples ttl-file "turtle")))
      (assert-equal expected-nt nt1 "turtle -> ntriples"))

    ;; Exercise FFI with syntax guessing
    (let ((nt2 (raptor-parse-file->ntriples ttl-file "guess")))
      (assert-equal expected-nt nt2 "guess -> ntriples"))

    ;; Clean up
    (when (file-exists? ttl-file)
      (delete-file ttl-file))

    (display "All tests passed.
")))
```

This code exports main and imports the FFI wrapper. Utility helpers include write-file (persist a string to disk), read-file (characterwise file read; defined but unused here), and assert-equal, which prints PASS/FAIL labels and exits with non-zero status on mismatch. In main, a small Turtle document defines a simple triple using the ex: prefix; the corresponding expected N-Triples string is the fully expanded IRI form with a terminating period and newline.

The test proceeds in two phases: first it calls **raptor-parse-file->ntriples ttl-file** "turtle" and checks the result; then it repeats using "guess" to confirm the parser’s auto-detection path yields identical serialization. After both assertions pass, it deletes the temporary file and prints “All tests passed.” The result is a minimal but effective smoke test verifying the FFI, Raptor’s parsing/serialization, and the contract that both explicit syntax selection and guessing produce stable N-Triples output.

We use a Makefile to build an executable:

```makefile
RAPTOR_PREFIX ?= /opt/homebrew/Cellar/raptor/2.0.16
OPENSSL_PREFIX ?= /opt/homebrew/opt/openssl@3

# Add raptor2 includes and libs; also ensure the correct
# OpenSSL lib dir is on the link path so -lssl/-lcrypto
# resolve even if Gerbil was built against a different
# Homebrew Cellar version.
CC_OPTS := -I$(RAPTOR_PREFIX)/include/raptor2
LD_OPTS := -L$(RAPTOR_PREFIX)/lib -lraptor2 -L$(OPENSSL_PREFIX)/lib

build:
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" ffi.ss
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" -exe -o test test.ss

clean:
	rm -f *.c *.scm *.o *.so test
```

Test output is:

```
$ ./test
PASS turtle -> ntriples
PASS guess -> ntriples
All tests passed.
```


