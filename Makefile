REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = apps/hg_proto/damsel
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

.PHONY: all submodules compile devrel start test clean distclean dialyze release containerize

all: compile

rebar-update:
	$(REBAR) update

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules
	$(REBAR) compile

devrel: submodules
	$(REBAR) release

start: submodules
	$(REBAR) run

test: submodules
	$(REBAR) ct

lint: compile
	elvis rock

xref: submodules
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

dialyze:
	$(REBAR) dialyzer

DOCKER := $(shell which docker 2>/dev/null)
PACKER := $(shell which packer 2>/dev/null)
BASE_DIR := $(shell pwd)

release: $(DOCKER) ~/.docker/config.json distclean
	$(DOCKER) run --rm -v $(BASE_DIR):$(BASE_DIR) --workdir $(BASE_DIR) rbkmoney/build rebar3 as prod release

containerize: $(PACKER) release ./packer.json
	$(PACKER) build packer.json

~/.docker/config.json:
	test -f ~/.docker/config.json || (echo "Please run: docker login" ; exit 1)
