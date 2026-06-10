# Makefile for Chess Engine & AI in Gerbil Scheme

.PHONY: run test compile clean

# Run the interactive CLI chess game
run:
	gxi cli.ss

# Run the performance and correctness tests (perft)
test:
	gxi perft.ss

# Compile the source files into optimized bytecode (using gxc)
compile:
	gxc engine.ss ai.ss

# Clean any compiled compilation artifacts
clean:
	rm -rf .gerbil/
