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
    image: dr.rbkmoney.com/rbkmoney/dominant:ab8b5e8d4343d10a2f5e20dc40fa54b49d15b8a9
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:bde2440a87e8311b6e2db90e915f8efdaa520ba1
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 8s
      timeout: 4s

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:ef494632710c3248a7d6a33fcbeb7944ce8fdd31
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
      interval: 8s
      timeout: 4s

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

