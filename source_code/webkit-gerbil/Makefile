# webkit-gerbil Makefile
# Builds libwebkit_cl.dylib and compiles Gerbil FFI bindings

CC = clang
CFLAGS = -fobjc-arc -fPIC -O2 -Wall
FRAMEWORKS = -framework Cocoa -framework WebKit
DYNLIB_LDFLAGS = -dynamiclib -install_name @rpath/libwebkit_cl.dylib

GXC ?= gxc

DYLIB = libwebkit_cl.dylib
SRC_M = webkit_cl.m

# Gerbil compile flags — link against the dylib
GERBIL_CC_OPTS = -I$(CURDIR)
GERBIL_LD_OPTS = -L$(CURDIR) -lwebkit_cl -Wl,-rpath,@loader_path \
                 -framework Cocoa -framework WebKit

.PHONY: all clean dylib examples hello counter viewer

all: dylib examples

# ── Build the Objective-C dynamic library ────────────────────────

dylib: $(DYLIB)

$(DYLIB): $(SRC_M) webkit_cl.h
	$(CC) $(CFLAGS) $(FRAMEWORKS) $(DYNLIB_LDFLAGS) -o $@ $<

# ── Compile examples as standalone executables ───────────────────
# FFI code must be compiled (gxi can't interpret c-declare),
# so we build standalone executables via gxc.

examples: hello counter viewer

hello: $(DYLIB)
	$(GXC) -cc-options "$(GERBIL_CC_OPTS)" \
	       -ld-options "$(GERBIL_LD_OPTS)" \
	       -exe -o hello-world \
	       ffi.ss webkit-gerbil.ss examples/hello-world.ss

counter: $(DYLIB)
	$(GXC) -cc-options "$(GERBIL_CC_OPTS)" \
	       -ld-options "$(GERBIL_LD_OPTS)" \
	       -exe -o counter-app \
	       ffi.ss webkit-gerbil.ss examples/counter-app.ss

viewer: $(DYLIB)
	$(GXC) -cc-options "$(GERBIL_CC_OPTS)" \
	       -ld-options "$(GERBIL_LD_OPTS)" \
	       -exe -o markdown-viewer \
	       ffi.ss webkit-gerbil.ss examples/markdown-viewer.ss

# ── Clean ────────────────────────────────────────────────────────

clean:
	rm -f $(DYLIB) hello-world counter-app markdown-viewer
	rm -f *.c *.o *.so *.o1 *.o2 *.ssi
