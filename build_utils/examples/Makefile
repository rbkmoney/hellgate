## Mandatory variables

# For convenience (specific for this repo)
# BASE_PATH is defined in the top Makefile
UTILS_PATH := $(BASE_PATH)/make_lib
TEMPLATES_PATH := $(BASE_PATH)/examples

# Name of the service
SERVICE_NAME := build_utils_demo


## Variables required for utils_image.mk

# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service_base
BASE_IMAGE_TAG := 6de9b3b


## Variables required for utils_container.mk

# Build image tag to be used
BUILD_IMAGE_TAG := 530114ab63a7ff0379a2220169a0be61d3f7c64c

CALL_W_CONTAINER := all smoke_test


## Targets

.PHONY: $(CALL_W_CONTAINER)

all: smoke_test

smoke_test:
	echo OK

include $(UTILS_PATH)/utils_container.mk
include $(UTILS_PATH)/utils_image.mk

