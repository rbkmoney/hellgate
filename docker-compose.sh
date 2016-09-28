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
    image: dr.rbkmoney.com/rbkmoney/dominant:3f6ba2c78d2e16b0ad944382c3e8129ea2fc4e20
    depends_on:
      - machinegun

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:a48f9e93dd5a709d5f14db0c9785d43039282e86
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:b9487a2313ede02780a90895eb74d43e57b931f6
    entrypoint: |
      java
      -Xmx512m
      -jar
      /opt/shumway/shumway-0.0.1-SNAPSHOT.jar
    command: |
      --spring.datasource.url=jdbc:postgresql://shumway_psql:5432/shumway
      --spring.datasource.username=shumway
      --spring.datasource.password=shumway
    depends_on:
      - shumway_psql
  shumway_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=shumway
      - POSTGRES_USER=shumway
      - POSTGRES_PASSWORD=shumway

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF

