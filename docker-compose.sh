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
      - riakdb

  riakdb:
    image: dr.rbkmoney.com/rbkmoney/riak-kv:a5de2b08925bcf676a0b30b202dac122d0da5769
    entrypoint: "sh -c \$\$RIAK_HOME/riak-cluster.sh"
    ports:
      - "8087:8087"
      - "8098:8098"
    environment:
      - CLUSTER_NAME=riakkv
    labels:
      - "com.basho.riak.cluster.name=riakkv"
    volumes:
      - schemas:/etc/riak/schemas

  member:
    image: dr.rbkmoney.com/rbkmoney/riak-kv:a5de2b08925bcf676a0b30b202dac122d0da5769
    entrypoint: "sh -c \$\$RIAK_HOME/riak-cluster.sh"
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
  schemas:
    external: false
networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF

