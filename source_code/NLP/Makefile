GERBIL_PATH := $(CURDIR)/.gerbil
export GERBIL_PATH

all: build

build:
	gxi build.ss

.PHONY: test test-fast

test: compile-mods
	@echo "Running smoke test (gxi) on climate_g8.txt..."
	@mkdir -p .gerbil
	@rm -rf .gerbil/test-output.json
	gxi nlp.ss -- -i data/testdata/climate_g8.txt -o .gerbil/test-output.json
	@echo "Wrote .gerbil/test-output.json"
	@/bin/echo -n "Preview: " && head -c 300 .gerbil/test-output.json || true

test-fast: test

clean:
	@echo "Cleaning project build artifacts..."
	@rm -rf .gerbil
	@rm -f nlp a.out
	@find . -maxdepth 1 -type f \( -name "*.o*" -o -name "*.ssxi" -o -name "*.ssi" \) -delete

.PHONY: compile-mods
compile-mods:
	@echo "Compiling modules with gxc into project-local .gerbil..."
	gxc utils.ss fasttag.ss category.ss proper-names.ss \
	  main.ss nlp.ss
