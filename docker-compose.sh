#!/bin/bash
cat <<EOF
version: '2.1'

services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
      - $HOME/.ssh:/home/$UNAME/.ssh:ro
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      riakdb:
        condition: service_healthy
      kds:
        condition: service_healthy
      oldcds:
        condition: service_healthy

  oldcds:
    image: dr2.rbkmoney.com/rbkmoney/cds:9d02af704ba5e3e8c0dee902a951f89c34e65bb7
    command: /opt/cds/bin/cds foreground
    depends_on:
      riakdb:
        condition: service_healthy
      kds:
        condition: service_healthy
    volumes:
      - ./test/oldcds/sys.config:/opt/cds/releases/0.1.0/sys.config
      - ./test/oldcds/ca.crt:/var/lib/cds/ca.crt:ro
      - ./test/oldcds/client.pem:/var/lib/cds/client.pem:ro

    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  riakdb:
    image: dr2.rbkmoney.com/rbkmoney/riak-base:d9dec1c4a69482f5c013bb155f6ccd18cd9d4653
    environment:
      - CLUSTER_NAME=riakkv
    labels:
      - "com.basho.riak.cluster.name=riakkv"
    volumes:
      - ./test/riak/user.conf:/etc/riak/user.conf:ro
    healthcheck:
      test: "riak-admin test"
      interval: 5s
      timeout: 10s
      retries: 20
  member:
    image: dr2.rbkmoney.com/rbkmoney/riak-base:d9dec1c4a69482f5c013bb155f6ccd18cd9d4653
    labels:
      - "com.basho.riak.cluster.name=riakkv"
    links:
      - riakdb
    depends_on:
      - riakdb
    environment:
      - CLUSTER_NAME=riakkv
      - COORDINATOR_NODE=riakdb
    volumes:
      - ./test/riak/user.conf:/etc/riak/user.conf:ro

  kds:
    image: dr2.rbkmoney.com/rbkmoney/kds:2eba263c863a3137036480718a922394a7215375
    command: /opt/kds/bin/kds foreground
    volumes:
      - ./test/kds/sys.config:/opt/kds/releases/0.1.0/sys.config:ro
      - ./test/kds/ca.crt:/var/lib/kds/ca.crt:ro
      - ./test/kds/server.pem:/var/lib/kds/server.pem:ro
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

volumes:
  schemas:
    external: false

EOF

