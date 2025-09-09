all: build

build:
	# Redirect HOME so gxpkg installs into project-local .gerbil
	HOME=$(CURDIR) gxi build.ss

.PHONY: test test-fast

test: compile-mods
	@echo "Running smoke test (gxi) on climate_g8.txt..."
	@mkdir -p .gerbil
	@rm -rf .gerbil/test-output.json
	@HOME=$(CURDIR) gxi nlp.ss -- -i data/testdata/climate_g8.txt -o .gerbil/test-output.json
	@echo "Wrote .gerbil/test-output.json"
	@/bin/echo -n "Preview: " && head -c 300 .gerbil/test-output.json || true

# Interpreter-based run; useful if static exe build is problematic
test-fast: compile-mods
	@echo "Running interpreter smoke test (gxi) on climate_g8.txt..."
	@mkdir -p .gerbil
	@HOME=$(CURDIR) gxi nlp.ss -- -i data/testdata/climate_g8.txt -o .gerbil/test-output.json
	@echo "Wrote .gerbil/test-output.json"
	@/bin/echo -n "Preview: " && head -c 300 .gerbil/test-output.json || true


clean:
	@echo "Cleaning project build artifacts..."
	@rm -rf .gerbil
	@rm -f kbtm nlp a.out
	@find . -maxdepth 1 -type f \( -name "*.o*" -o -name "*.ssxi" -o -name "*.ssi" \) -delete
.PHONY: compile-mods
compile-mods:
	@echo "Compiling modules with gxc into project-local .gerbil..."
	@HOME=$(CURDIR) gxc utils.ss fasttag.ss category.ss proper-names.ss \
	  data/stop-words.ss generated-code/lexdata.ss generated-code/cat-data-tables.ss \
	  main.ss nlp.ss
