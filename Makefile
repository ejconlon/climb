include Makefile.base

.PHONY: demo
demo: build
	stack exec -- climb-demo
