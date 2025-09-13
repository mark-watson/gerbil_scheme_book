# Command Line Application For A LLM Based RAG System For Search And Chatting With Local Documents

* work in progress*

We will need to calculate text embeddings for documents we index, store these embedding in a database, and efficient;y look up docments by embeddings.

## First Step: Storing Text Embeddings in Sqlite

We will be using this Gerbil Scheme documentation page: [https://gerbil.scheme.org/reference/std/db/](https://gerbil.scheme.org/reference/std/db/).

We have several options for storing embeddings:

- Minimal: store embeddings; no ANN index using either JSON-encoded floats or packed bytes in a blob object.
- Better: use a SQLite vector extension from Gerbil

We have several options for vector extensions:

- sqlite-vss: Build sqlite-vss for your platform (shared library). 2) Enable extension loading and SELECT load_extension(...). 3) Create a VSS table and upsert.
- sqlite-vec (small, modern): sqlite-vec uses a vec table and functions like vec_search.


