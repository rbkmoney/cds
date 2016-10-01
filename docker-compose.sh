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
      - riak1
      - riak2
      - riak3
      - riak4
      - riak5
  riak1:
    image: hectcastro/riak
    environment:
      - DOCKER_RIAK_CLUSTER_SIZE=5
      - DOCKER_RIAK_AUTOMATIC_CLUSTERING=1
      - DOCKER_RIAK_BACKEND=bitcask
      - DOCKER_RIAK_STRONG_CONSISTENCY=off
  riak2:
    image: hectcastro/riak
    links:
      - riak1:seed
    environment:
      - DOCKER_RIAK_CLUSTER_SIZE=5
      - DOCKER_RIAK_AUTOMATIC_CLUSTERING=1
      - DOCKER_RIAK_BACKEND=bitcask
      - DOCKER_RIAK_STRONG_CONSISTENCY=off
  riak3:
    image: hectcastro/riak
    links:
      - riak1:seed
    environment:
      - DOCKER_RIAK_CLUSTER_SIZE=5
      - DOCKER_RIAK_AUTOMATIC_CLUSTERING=1
      - DOCKER_RIAK_BACKEND=bitcask
      - DOCKER_RIAK_STRONG_CONSISTENCY=off
  riak4:
    image: hectcastro/riak
    links:
      - riak1:seed
    environment:
      - DOCKER_RIAK_CLUSTER_SIZE=5
      - DOCKER_RIAK_AUTOMATIC_CLUSTERING=1
      - DOCKER_RIAK_BACKEND=bitcask
      - DOCKER_RIAK_STRONG_CONSISTENCY=off
  riak5:
    image: hectcastro/riak
    links:
      - riak1:seed
    environment:
      - DOCKER_RIAK_CLUSTER_SIZE=5
      - DOCKER_RIAK_AUTOMATIC_CLUSTERING=1
      - DOCKER_RIAK_BACKEND=bitcask
      - DOCKER_RIAK_STRONG_CONSISTENCY=off
EOF

