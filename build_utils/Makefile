BASE_PATH := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

include examples/Makefile

clean:
	rm Dockerfile docker-compose.yml
