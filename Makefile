REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = apps/hg_proto/damsel
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

REGISTRY := dr.rbkmoney.com
ORG_NAME := rbkmoney
BASE_IMAGE := "$(REGISTRY)/$(ORG_NAME)/build:latest"

# Note: RELNAME should match the name of
# the first service in docker-compose.yml
RELNAME := hellgate

IMAGE_TAG ?= $(shell whoami)
PUSH_IMAGE_TAG ?= $(IMAGE_TAG)
IMAGE_NAME = $(REGISTRY)/$(ORG_NAME)/$(RELNAME)

CALL_ANYWHERE := submodules rebar-update compile xref lint dialyze start devrel release clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

include utils.mk

.PHONY: $(CALL_W_CONTAINER) all containerize push $(UTIL_TARGETS)

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint: compile
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

# CALL_W_CONTAINER
test: submodules
	$(REBAR) ct

# OTHER
containerize: wc_release
	$(DOCKER) build --force-rm --tag "$(IMAGE_NAME):$(IMAGE_TAG)" .

push:
	$(DOCKER) tag "$(IMAGE_NAME):$(IMAGE_TAG)" "$(IMAGE_NAME):$(PUSH_IMAGE_TAG)"
	$(DOCKER) push "$(IMAGE_NAME):$(PUSH_IMAGE_TAG)"

