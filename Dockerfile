FROM rbkmoney/service_erlang:latest
MAINTAINER Igor Savchuk <i.savchuk@rbkmoney.com>
COPY _build/prod/rel/cds /opt/cds
CMD ["/opt/cds/bin/cds", "foreground"]
LABEL service_version="semver"
WORKDIR /opt/cds
