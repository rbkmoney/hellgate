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

  shumpune:
    image: dr2.rbkmoney.com/rbkmoney/shumpune:d4c6618754b45d128be595d8813ed45f20c00b10
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

