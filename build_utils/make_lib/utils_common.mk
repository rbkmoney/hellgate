ifndef SERVICE_NAME
$(error SERVICE_NAME is not set)
endif

REGISTRY := dr.rbkmoney.com
ORG_NAME := rbkmoney

SHELL := /bin/bash

which = $(if $(shell which $(1) 2>/dev/null),\
	$(shell which $(1) 2>/dev/null),\
	$(error "Error: could not locate $(1)!"))

DOCKER = $(call which,docker)

validate_templates_path = $(shell if [ -n "$(TEMPLATES_PATH)" ]; then \
	if [ ! -d "$(TEMPLATES_PATH)" ]; then \
	echo "Error: $(TEMPLATES_PATH) does not exist!" && exit 1; \
	fi; \
	else \
	echo "Error: TEMPLATES_PATH is not set!" && exit 1; \
	fi)

