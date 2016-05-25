REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
DOCKER := $(shell which docker)
GIT := $(shell which git)
PACKER := $(shell which packer)
PWD := $(shell pwd)
RELNAME = cds

.PHONY: all compile devrel start test clean distclean dialyze damsel

all: compile

compile: damsel
	$(REBAR) compile

damsel: $(GIT)
	$(GIT) submodule update --init apps/cds/damsel

rebar-update:
	$(REBAR) update

devrel:
	$(REBAR) release

start: devrel
	$(REBAR) run

test:
	$(REBAR) ct

xref:
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

dialyze:
	$(REBAR) dialyzer

release: $(DOCKER) distclean ~/.docker/config.json
	$(DOCKER) run --rm -v ~/.ssh:/root/.ssh -v $(PWD):$(PWD) --workdir $(PWD) rbkmoney/build_erlang rebar3 as prod release

containerize: $(PACKER) release ./packer.json
	$(PACKER) build packer.json

~/.docker/config.json:
	test -f ~/.docker/config.json || (echo "Please run: docker login" ; exit 1)
