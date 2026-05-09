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

;;; ---- High-level Scheme API --------------------------------------------------

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

;; Parse an RDF file and return N-Triples as a string.
;; SYNTAX can be "turtle", "rdfxml", "ntriples", or "guess" (default).
;; Signals an error if parsing fails.
(def (parse-rdf-file filename (syntax "guess"))
  (let ((result (raptor-parse-file->ntriples filename syntax)))
    (if result
        result
        (error "Failed to parse RDF file" filename syntax))))

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
