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

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:4c29acdcdce065dbba1f3c8ee1683caea837869c
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF

