#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      machinegun:
        condition: service_healthy
      shumway:
        condition: service_healthy

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:0e8dc6a9335c832fe4394768f24f5c74301ba7cb
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

  sequences:
    image: dr.rbkmoney.com/rbkmoney/sequences:727c81115f861dc3d9b80c0e06e64d27728d447f
    command: /opt/sequences/bin/sequences foreground
    depends_on:
      machinegun:
        condition: service_healthy

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:1844dff663c24acdcd32f30ae3ea208f5d05a008
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:7a5f95ee1e8baa42fdee9c08cc0ae96cd7187d55
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    depends_on:
      - shumway-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  shumway-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db
EOF

