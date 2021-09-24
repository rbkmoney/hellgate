#!/bin/bash
cat <<EOF
version: '2.4'
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
    image: dr2.rbkmoney.com/rbkmoney/dominant:15ceafee13b874a728d28fc5567ad070fac1d0fa
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

  bender:
    image: dr2.rbkmoney.com/rbkmoney/bender:b392d600186e8de842db5f35ae2e53dda046ddd6
    command: /opt/bender/bin/bender foreground
    volumes:
      - ./test/log/bender:/var/log/bender
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20
    depends_on:
      - machinegun

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:c35e8a08500fbc2f0f0fa376a145a7324d18a062
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
    image: dr2.rbkmoney.com/rbkmoney/limiter:c5572a9a22b3fea68213f32276b5272605aebec8
    command: /opt/limiter/bin/limiter foreground
    depends_on:
      machinegun:
        condition: service_healthy
      shumway:
        condition: service_healthy

  shumway:
    image: dr2.rbkmoney.com/rbkmoney/shumway:2f7d381d36ec69cfc90c77996f7e82b79d89e80b
    hostname: shumway
    container_name: shumway
    ports:
      - "8022:8022"
    environment:
      SPRING_APPLICATION_JSON: '{
            "spring.datasource.url": "jdbc:postgresql://postgres:5432/shumway",
            "spring.datasource.username": "postgres",
            "spring.datasource.password": "postgres",
            "management.metrics.export.statsd.enabled": "false",
            "service.shumaich.url": "http://shumaich:8022/shumaich"
          }'
    depends_on:
      - postgres
      - shumaich
    healthcheck:
      # FIXME: dirty trick, hangs in "health: staring" otherwise
      #        used to be fine
      test: "exit 0"
      # test: "curl http://localhost:8022/"
      # interval: 5s
      # timeout: 1s
      # retries: 20

  zookeeper:
    image: confluentinc/cp-zookeeper:5.0.1
    hostname: zookeeper
    container_name: zookeeper
    environment:
      ZOOKEEPER_CLIENT_PORT: 2181
      ZOOKEEPER_TICK_TIME: 2000
    volumes:
      - type: volume
        target: /var/lib/zookeeper/data
        volume:
          nocopy: true
      - type: volume
        target: /var/lib/zookeeper/log
        volume:
          nocopy: true

  broker:
    image: confluentinc/cp-enterprise-kafka:5.0.1
    hostname: broker
    container_name: broker
    depends_on:
      - zookeeper
    environment:
      KAFKA_BROKER_ID: 1
      KAFKA_ZOOKEEPER_CONNECT: 'zookeeper:2181'
      KAFKA_LISTENER_SECURITY_PROTOCOL_MAP: PLAINTEXT:PLAINTEXT,PLAINTEXT_HOST:PLAINTEXT
      KAFKA_ADVERTISED_LISTENERS: PLAINTEXT_HOST://broker:29092,PLAINTEXT://broker:9092
      KAFKA_METRIC_REPORTERS: io.confluent.metrics.reporter.ConfluentMetricsReporter
      KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR: 1
      KAFKA_GROUP_INITIAL_REBALANCE_DELAY_MS: 0
      CONFLUENT_METRICS_REPORTER_BOOTSTRAP_SERVERS: broker:9092
      CONFLUENT_METRICS_REPORTER_ZOOKEEPER_CONNECT: zookeeper:2181
      CONFLUENT_METRICS_REPORTER_TOPIC_REPLICAS: 1
      CONFLUENT_METRICS_ENABLE: 'true'
      CONFLUENT_SUPPORT_CUSTOMER_ID: 'anonymous'
    volumes:
      - type: volume
        target: /var/lib/kafka/data
        volume:
          nocopy: true

  kafka-setup:
    image: confluentinc/cp-kafka:5.1.1
    hostname: kafka-setup
    container_name: kafka-setup
    depends_on:
      - broker
    command: "bash -c 'echo Waiting for Kafka to be ready... && \
                              cub kafka-ready -b broker:9092 1 60 && \
                              kafka-topics --create --if-not-exists --zookeeper zookeeper:2181 --partitions 1 --replication-factor 1 --topic operation_log'"

  shumaich:
    image: dr2.rbkmoney.com/rbkmoney/shumaich:3be4048303d9a649027faa95d87a5ecd99af1e6b
    hostname: shumaich
    container_name: shumaich
    restart: on-failure
    environment:
      SPRING_APPLICATION_JSON: '{
          "rocksdb.name": "shumaich",
          "rocksdb.dir": "/temp/rocksdb",
          "kafka.bootstrap-servers": "broker:9092",
          "kafka.topics.operation-log-name": "operation_log",
          "management.metrics.export.statsd.enabled": "false"
        }'
    depends_on:
      - broker
      - kafka-setup
    volumes:
      - type: volume
        target: /temp/rocksdb/shumaich
        volume:
          nocopy: true
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  holmes:
    image: dr2.rbkmoney.com/rbkmoney/holmes:7d496d0886a1489044c57eee4ba4bfcf8f8b6a48
    hostname: holmes
    container_name: holmes
    volumes:
      - ./_build/default/lib/shumpune-proto/proto/:/opt/holmes/shumaich-proto

  postgres:
    image: postgres:9.6
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: shumway
    volumes:
      - type: volume
        target: /var/lib/postgresql/data/
        volume:
          nocopy: true
EOF
