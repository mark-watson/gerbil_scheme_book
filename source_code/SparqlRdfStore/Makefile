# Makefile — builds rdfwrap from C-source/wrapper.c
# Homebrew-style prefixes (override on CLI if needed)
SORD_PREFIX    ?= /opt/homebrew/opt/sord
SERD_PREFIX    ?= /opt/homebrew/opt/serd
RASQAL_PREFIX  ?= /opt/homebrew/opt/rasqal
RAPTOR_PREFIX  ?= /opt/homebrew/opt/raptor
OPENSSL_PREFIX ?= /opt/homebrew/opt/openssl@3

# Toolchain
CC        ?= cc
PKG_CONFIG?= pkg-config

# Sources/outputs
SRC    := C-source/wrapper.c
OBJ    := $(SRC:.c=.o)
TARGET := rdfwrap

# Prefix-based flags (always added)
PREFIX_CFLAGS := \
  -I$(SORD_PREFIX)/include/sord-0 \
  -I$(SERD_PREFIX)/include/serd-0 \
  -I$(RASQAL_PREFIX)/include \
  -I$(RAPTOR_PREFIX)/include/raptor2

PREFIX_LDFLAGS := \
  -L$(SORD_PREFIX)/lib -lsord-0 \
  -L$(SERD_PREFIX)/lib -lserd-0 \
  -L$(RASQAL_PREFIX)/lib -lrasqal \
  -L$(RAPTOR_PREFIX)/lib -lraptor2 \
  -L$(OPENSSL_PREFIX)/lib

# pkg-config flags (added if available)
HAVE_PKGCFG := $(shell $(PKG_CONFIG) --exists sord-0 serd-0 rasqal raptor2 && echo yes || echo no)
PKG_CFLAGS  := $(if $(filter yes,$(HAVE_PKGCFG)),$(shell $(PKG_CONFIG) --cflags sord-0 serd-0 rasqal raptor2))
PKG_LDFLAGS := $(if $(filter yes,$(HAVE_PKGCFG)),$(shell $(PKG_CONFIG) --libs   sord-0 serd-0 rasqal raptor2))

# Final flags (prefixes first, then pkg-config, then user overrides)
# add a demo main() fuction also:
CFLAGS  ?= -Wall -O2 -DRDF_DEMO_MAIN
CFLAGS  += $(PREFIX_CFLAGS) $(PKG_CFLAGS)
LDFLAGS += $(PREFIX_LDFLAGS) $(PKG_LDFLAGS)

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ) $(TARGET)

print-flags:
	@echo "CFLAGS = $(CFLAGS)"
	@echo "LDFLAGS = $(LDFLAGS)"
	@echo "HAVE_PKGCFG = $(HAVE_PKGCFG)"

.PHONY: all clean print-flags