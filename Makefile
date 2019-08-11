.PHONY: all build clean test examples

build:
	dune build @install

all: build

doc:
	dune build @doc

test:
	dune runtest

examples:
	dune build @examples

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
