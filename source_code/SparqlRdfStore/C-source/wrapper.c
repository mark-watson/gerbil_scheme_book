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
   Compile by adding -DRDF_DEMO_MAIN to CFLAGS, then run:
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