.PHONY: clean test all-test build doc

build:
	dune build

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean
