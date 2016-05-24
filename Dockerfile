FROM buildpack-deps:jessie

MAINTAINER kittee

COPY _build/prod/rel/cds /usr/local/cds

EXPOSE 8022

CMD ["/usr/local/cds/bin/cds", "foreground"]
