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
