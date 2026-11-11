ARCH := $(shell uname -m)

GXP ?= $(shell command -v gxpkg 2>/dev/null)
ifeq ($(strip $(GXP)),)
  $(error gxpkg not found in PATH. Install Gerbil or run with GXP=/path/to/gxpkg)
endif

default: build

test:
	gxi test-cache.ss

build:
	$(GXP) deps -i
	$(GXP) build

build-release:
	$(GXP) deps -i
	$(GXP) build --release

install:
	install -m 755 .gerbil/bin/daily-use /usr/local/bin/daily-use

clean:
	gerbil clean
	gerbil clean all
