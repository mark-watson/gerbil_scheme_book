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
  (define-c-lambda rdf-free  ()           void        "rdf_free"))
