REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := hellgate
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := 0ac24d27225ff6a9d5cbf9ce7ba4a956a2b41a2d

# Build image tag to be used
BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := e6e7e3ad278ede660e512426940ea0613b595f2b

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze plt_update \
				release clean distclean format check_format

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

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

lint:
	elvis rock -V

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze: submodules
	$(REBAR) as test dialyzer

plt_update:
	$(REBAR) dialyzer -u true -s false


release: submodules
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rf _build

# CALL_W_CONTAINER
test: submodules
	$(REBAR) do eunit, ct

test.%: apps/hellgate/test/hg_%_tests_SUITE.erl
	$(REBAR) ct --suite=$^
