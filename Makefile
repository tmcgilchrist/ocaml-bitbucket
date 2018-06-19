.PHONY: build clean test

build:
	jbuilder build @install @DEFAULT

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install
