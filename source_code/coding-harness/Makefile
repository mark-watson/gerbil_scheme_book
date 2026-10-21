# Gerbil Coding Harness
#
#   make run        start the REPL (under rlwrap when installed: line editing + history)
#   make check      expand-compile every module with gxc -S, then run the tests
#   make test       run the tests only
#   make install    put a `gerbil-harness` launcher in $(PREFIX)/bin

SRCS := config.ss util.ss parens.ss approval.ss llm.ss checker.ss tools.ss harness.ss
PREFIX ?= /usr/local
HERE := $(shell pwd)
RLWRAP := $(shell command -v rlwrap 2>/dev/null)

.PHONY: run check test install uninstall clean help

help:
	@echo "targets: run check test install uninstall clean"

run:
	$(if $(RLWRAP),rlwrap -c -H $(HOME)/.gerbil_harness_history) gxi harness.ss

# gxc -S macro-expands and checks every module (unbound identifiers, bad
# special forms, unreadable files) without the slow C compile.
check:
	@mkdir -p .gerbil/check
	gxc -S -d .gerbil/check $(SRCS)
	gxi test/parens-test.ss

test:
	gxi test/parens-test.ss

install:
	@mkdir -p $(DESTDIR)$(PREFIX)/bin
	@printf '#!/bin/sh\nexec $(if $(RLWRAP),rlwrap -c -H "$$HOME/.gerbil_harness_history" )gxi "$(HERE)/harness.ss" "$$@"\n' > $(DESTDIR)$(PREFIX)/bin/gerbil-harness
	@chmod 0755 $(DESTDIR)$(PREFIX)/bin/gerbil-harness
	@echo "Installed $(DESTDIR)$(PREFIX)/bin/gerbil-harness -> $(HERE)/harness.ss"

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/gerbil-harness

clean:
	rm -rf .gerbil *~
