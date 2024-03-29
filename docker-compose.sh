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
    mem_limit: 512M

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:ea1a6680e3a68db92b485102e5c1c9525c9d874f
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:9c3248a68fe530d23a8266057a40a1a339a161b8
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
      - ./test/machinegun/cookie:/opt/machinegun/etc/cookie
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  limiter:
    image: dr2.rbkmoney.com/rbkmoney/limiter:c7e96068a56da444e78cc7739a902da8e268dc63
    command: /opt/limiter/bin/limiter foreground
    depends_on:
      machinegun:
        condition: service_healthy
      shumway:
        condition: service_healthy

  shumway:
    image: dr2.rbkmoney.com/rbkmoney/shumway:658c9aec229b5a70d745a49cb938bb1a132b5ca2
    restart: unless-stopped
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --management.metrics.export.statsd.enabled=false
    depends_on:
      - shumway-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  party-management:
    image: dr2.rbkmoney.com/rbkmoney/party-management:ac0feed5de13797941c3a576c04b0a5b7302c549
    command: /opt/party-management/bin/party-management foreground
    depends_on:
      - machinegun
      - dominant
      - shumway
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  shumway-db:
    image: dr2.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db
EOF
