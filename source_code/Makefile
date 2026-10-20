# Top-level Makefile for Gerbil Scheme Book source_code
# Iterates across all example subdirectories.

SUBDIRS := $(wildcard */)

.PHONY: clean all

clean:
	@for dir in $(SUBDIRS); do \
	  if [ -f "$$dir/Makefile" ]; then \
	    echo "==> Cleaning $$dir"; \
	    $(MAKE) -C "$$dir" clean || true; \
	  fi; \
	done

all:
	@echo "Run 'make clean' to clean all subdirectories."
