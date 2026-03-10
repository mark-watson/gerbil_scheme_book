.PHONY: all run compile clean

all: run

## Run the demo directly with the Gerbil interpreter (no compilation needed)
run:
	gxi RDF.ss

## Compile the module with gxc (generates .o / .ssxi artefacts in .gerbil/)
compile:
	@echo "Compiling RDF.ss with gxc..."
	gxc RDF.ss
	@echo "Done. You can now import :rdf-datastore/RDF from gxi."

## Remove compiled artefacts
clean:
	@echo "Cleaning build artefacts..."
	@rm -rf .gerbil
	@find . -maxdepth 1 -type f \( -name "*.o*" -o -name "*.ssxi" -o -name "*.ssi" \) -delete
	@echo "Done."
