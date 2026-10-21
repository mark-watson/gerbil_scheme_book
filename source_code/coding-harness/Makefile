# Coding Harness (written in Gerbil Scheme; works on Gerbil, Python, Haskell,
# TypeScript and JavaScript projects)
#
#   make run        start the REPL (under rlwrap when installed: line editing + history)
#   make check      expand-compile every module with gxc -S, then run the tests
#   make test       run the tests only
#   make install    put a launcher named $(NAME) in $(PREFIX)/bin
#                   (make install NAME=coding-harness to pick another name)

SRCS := config.ss util.ss parens.ss lang.ss approval.ss llm.ss checker.ss tools.ss harness.ss
PREFIX ?= ~/bin
NAME ?= gerbil-harness
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
	@printf '#!/bin/sh\nexec $(if $(RLWRAP),rlwrap -c -H "$$HOME/.gerbil_harness_history" )gxi "$(HERE)/harness.ss" "$$@"\n' > $(DESTDIR)$(PREFIX)/bin/$(NAME)
	@chmod 0755 $(DESTDIR)$(PREFIX)/bin/$(NAME)
	@echo "Installed $(DESTDIR)$(PREFIX)/bin/$(NAME) -> $(HERE)/harness.ss"

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/$(NAME)

clean:
	rm -rf .gerbil *~
