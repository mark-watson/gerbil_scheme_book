# Top-level Makefile for Gerbil Scheme Book source_code
# Iterates across all example subdirectories.

OFFLINE_TEST_DIRS := \
	shared \
	command_line_utilities_first_demo_START_HERE \
	RDF_datastore \
	web-scraping \
	anomaly-detection \
	Kmeans \
	chess-game \
	daily_use \
	NLP \
	command_line_NLP \
	Probability \
	autocontext \
	symbolic-math \
	text-adventure-game \
	coding-harness

SUBDIRS := $(wildcard */)

.PHONY: test clean all $(OFFLINE_TEST_DIRS)

all: test

test:
	@for dir in $(OFFLINE_TEST_DIRS); do \
	  echo "=================================================="; \
	  echo "==> Running tests in $$dir"; \
	  echo "=================================================="; \
	  $(MAKE) -C "$$dir" test || exit 1; \
	done; \
	echo ""; \
	echo "All offline tests PASSED!"

clean:
	@for dir in $(SUBDIRS); do \
	  if [ -f "$$dir/Makefile" ]; then \
	    echo "==> Cleaning $$dir"; \
	    $(MAKE) -C "$$dir" clean || true; \
	  fi; \
	done
