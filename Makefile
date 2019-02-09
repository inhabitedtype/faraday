.PHONY: all build clean test examples

build:
	jbuilder build @install --dev

all: build

doc:
	jbuilder build @doc

test:
	jbuilder runtest --dev

examples:
	jbuilder build @examples

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

