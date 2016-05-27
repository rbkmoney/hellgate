REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = apps/hg_proto/damsel
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

.PHONY: all submodules compile devrel start test clean distclean dialyze

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
