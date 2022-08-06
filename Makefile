include Makefile.base

.PHONY: demo
demo:
	stack build --test --no-run-tests --exec climb-demo
