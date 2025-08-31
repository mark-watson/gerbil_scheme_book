ARCH := $(shell uname -m)

# Resolve gxpkg from PATH, allow override via `make GXP=/path/to/gxpkg`
GXP ?= $(shell command -v gxpkg 2>/dev/null)
ifeq ($(strip $(GXP)),)
  $(error gxpkg not found in PATH. Install Gerbil or run with GXP=/path/to/gxpkg)
endif

default: build

build:
	$(GXP) deps -i
	$(GXP) build

build-release:
	$(GXP) deps -i
	$(GXP) build --release

install:
	mv .gerbil/bin/command_line_utilities /usr/local/bin/command_line_utilities

clean:
	gerbil clean
	gerbil clean all
