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
