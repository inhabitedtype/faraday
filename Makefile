.PHONY: all build clean test

build:
	jbuilder build @install --dev

all: build

test:
	jbuilder runtest --dev

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

