##### macOS:

#RAPTOR_PREFIX ?= /opt/homebrew/Cellar/raptor/2.0.16
#OPENSSL_PREFIX ?= /opt/homebrew/opt/openssl@3
#CC_OPTS := -I$(RAPTOR_PREFIX)/include/raptor2
#LD_OPTS := -L$(RAPTOR_PREFIX)/lib -lraptor2 -L$(OPENSSL_PREFIX)/lib

#### Ubuntu Linux
CC_OPTS += $(shell pkg-config --cflags raptor2)
LD_OPTS += $(shell pkg-config --libs raptor2)

# Linux:
#CFLAGS += -I/usr/include/raptor2

# Add raptor2 includes and libs; also ensure the correct OpenSSL lib dir
# is on the link path so -lssl/-lcrypto resolve even if Gerbil was built
# against a different Homebrew Cellar version.


build:
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" ffi.ss
	gxc -cc-options "$(CC_OPTS)" -ld-options "$(LD_OPTS)" -exe -o test test.ss

clean:
	rm -f *.c *.scm *.o *.so test
