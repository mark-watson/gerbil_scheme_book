##### macOS:
RAPTOR_PREFIX ?= /opt/homebrew/Cellar/raptor/2.0.16
OPENSSL_PREFIX ?= /opt/homebrew/opt/openssl@3
CC_OPTS := -I$(RAPTOR_PREFIX)/include/raptor2
LD_OPTS := -L$(RAPTOR_PREFIX)/lib -lraptor2 -L$(OPENSSL_PREFIX)/lib

build:
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" ffi.ss
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" -exe -o test test.ss

clean:
	rm -f *.c *.scm *.o *.so test
