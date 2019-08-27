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
      shumpune:
        condition: service_healthy
    mem_limit: 256M

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:7e1252e60e4965d03458113fd0c89d447f3520c0
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:aec434f47029dbd81762e10de04c9422e3c93e5e
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
    restart: unless-stopped
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

  shumpune:
    image: dr2.rbkmoney.com/rbkmoney/shumpune:7084098cdda5df734cc1ce1bc2dbf73442cd9288
    restart: unless-stopped
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumpune/shumpune.jar
      - --spring.datasource.url=jdbc:postgresql://shumpune-db:5432/shumpune
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    depends_on:
      - shumpune-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  shumpune-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumpune
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumpune-db
EOF

