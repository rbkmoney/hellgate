#!/bin/bash
cat <<EOF
version: '2'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      - machinegun
      - shumway

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:be25663099fc549b14ec6d4b72bc72a76d4e2a66
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      - machinegun

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:2c956c1172cf8f7b4a09512cd1571bdd4c57f1c1
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:ef494632710c3248a7d6a33fcbeb7944ce8fdd31
    restart: always
    command: |
      -Xmx512m
      -jar /opt/shumway/shumway.jar
      --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      --spring.datasource.username=postgres
      --spring.datasource.password=postgres
    depends_on:
      - shumway-db
    environment:
      - SERVICE_NAME=shumway
  shumway-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "true"
EOF

