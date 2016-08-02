# Building and pushing images

ifndef UTILS_PATH
$(error UTILS_PATH is not set)
endif

include $(UTILS_PATH)/utils_common.mk

ifndef SERVICE_IMAGE_TAG
$(error SERVICE_IMAGE_TAG is not set)
endif

# Image for this service
SERVICE_IMAGE_NAME = $(REGISTRY)/$(ORG_NAME)/$(SERVICE_NAME)


## Interface targets
build_image:
	$(MAKE) -s do_build_image

push_image:
	$(MAKE) -s do_push_image


## Utils
do_build_image: Dockerfile
	$(DOCKER) build --force-rm --tag $(SERVICE_IMAGE_NAME):$(SERVICE_IMAGE_TAG) . && \
	$(DOCKER) images | grep $(SERVICE_IMAGE_TAG)

do_push_image:
	$(if $(SERVICE_IMAGE_PUSH_TAG),,echo "SERVICE_IMAGE_PUSH_TAG is not set" ; exit 1)
	$(DOCKER) tag "$(SERVICE_IMAGE_NAME):$(SERVICE_IMAGE_TAG)" "$(SERVICE_IMAGE_NAME):$(SERVICE_IMAGE_PUSH_TAG)" && \
	$(DOCKER) push "$(SERVICE_IMAGE_NAME):$(SERVICE_IMAGE_PUSH_TAG)"

Dockerfile: $(call validate_templates_path) $(TEMPLATES_PATH)/Dockerfile.sh
	$(if $(BASE_IMAGE_NAME),,echo "BASE_IMAGE_NAME is not set" ; exit 1)
	$(if $(BASE_IMAGE_TAG),,echo "BASE_IMAGE_TAG is not set" ; exit 1)
	BASE_IMAGE="$(REGISTRY)/$(ORG_NAME)/$(BASE_IMAGE_NAME):$(BASE_IMAGE_TAG)" \
	BASE_IMAGE_TAG=$(BASE_IMAGE_TAG) \
	BUILD_IMAGE_TAG=$(BUILD_IMAGE_TAG) \
	$(TEMPLATES_PATH)/Dockerfile.sh > Dockerfile

