REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = apps/cds/damsel
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

ORG_NAME := rbkmoney
BASE_IMAGE := "$(ORG_NAME)/build:latest"
RELNAME := cds

TAG = latest
IMAGE_NAME = "$(ORG_NAME)/$(RELNAME):$(TAG)"

CALL_ANYWHERE := submodules rebar-update compile xref lint dialyze start devrel release clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

include utils.mk

.PHONY: $(CALL_W_CONTAINER) all containerize push $(UTIL_TARGETS)

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

deps: rebar-update
	$(REBAR) deps

rebar-update:
	$(REBAR) update

compile: submodules deps
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint: compile
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

# CALL_W_CONTAINER
test: submodules
	$(REBAR) ct

# OTHER
all: compile

containerize: w_container_release
	$(DOCKER) build --force-rm --tag $(IMAGE_NAME) .

push: containerize
	$(DOCKER) push "$(IMAGE_NAME)"