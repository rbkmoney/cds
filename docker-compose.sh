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

  riakdb:
    image: dr.rbkmoney.com/basho/riak-kv:ubuntu-2.1.4-1
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
      retries: 5

  member:
    image: dr.rbkmoney.com/basho/riak-kv:ubuntu-2.1.4-1
    ports:
      - "8087"
      - "8098"
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
    image: dr2.rbkmoney.com/rbkmoney/kds:a92c0a668795ed01c4100e11a5e2659cfe539d76
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

