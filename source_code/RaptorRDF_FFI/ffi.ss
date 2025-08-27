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
