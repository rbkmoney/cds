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
      - ./test/cds/sys.config:/opt/cds/releases/0.1.0/sys.config
      - ./test/cds/ca.crt:/var/lib/cds/ca.crt:ro
      - ./test/cds/client.pem:/var/lib/cds/client.pem:ro

    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  riakdb:
    image: dr.rbkmoney.com/basho/riak-kv:ubuntu-2.2.3
    environment:
      - CLUSTER_NAME=riakkv
    labels:
      - "com.basho.riak.cluster.name=riakkv"
    volumes:
      - ./test/riak/user.conf:/etc/riak/user.conf:ro
    healthcheck:
      test: "riak-admin test"
      interval: 5s
      timeout: 5s
      retries: 20
  member:
    image: dr.rbkmoney.com/basho/riak-kv:ubuntu-2.2.3
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
    image: dr2.rbkmoney.com/rbkmoney/kds:bbbf99db9636f9554f8bf092b268a2e479481943
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

