REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := cds
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := bdb3e60ddc70044bae1aa581d260d3a9803a2477

BUILD_IMAGE_TAG := fcf116dd775cc2e91bffb6a36835754e3f2d5321

CALL_W_CONTAINER := all submodules rebar-update compile xref lint dialyze test start devrel release clean distclean

DOCKER_COMPOSE_PREEXEC_HOOK = $(DOCKER_COMPOSE) scale member=4

.PHONY: $(CALL_W_CONTAINER)

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock

dialyze: submodules
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: submodules distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rf _build

test: submodules
	$(REBAR) do eunit,ct

