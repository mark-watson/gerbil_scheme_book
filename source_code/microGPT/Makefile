# Some Homebrew builds of Gerbil reference an OpenSSL path that no longer
# exists; adding the current Homebrew OpenSSL lib dir keeps linking working.
# Override with:  make exe LD_OPTS=
LD_OPTS ?= $(shell [ -d /opt/homebrew/opt/openssl@3/lib ] && echo "-L/opt/homebrew/opt/openssl@3/lib")

run: exe
	./microgpt 1000

exe:
	gxc -O -exe -o microgpt -ld-options "$(LD_OPTS)" microgpt.ss

# Interpreted, short run (about 0.25 s per training step)
test:
	gxi microgpt.ss 20

clean:
	rm -rf microgpt .gerbil *~
