.PHONY: build install uninstall test clean

build:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

test:
	sh test/nop/write-dune.sh > test/nop/cases/dune
	dune test

clean:
	dune clean
