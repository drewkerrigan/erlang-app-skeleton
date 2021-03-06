BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: deps

all: compile
compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
deps:
	$(REBAR) get-deps
cleantest:
	rm -rf .eunit/*
test: cleantest
	$(REBAR)  skip_deps=true eunit
rel: relclean deps compile
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
relclean:
	rm -rf rel/sampler
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/sampler/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/sampler/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/sampler/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/sampler/lib;)
