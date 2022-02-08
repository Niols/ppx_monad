.PHONY: build install uninstall test clean

build:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

test:
	sh test/nop/write-dune.sh > test/nop/dune
	dune test

clean:
	dune clean
	rm -f test/nop/dune
	rm *.opam
