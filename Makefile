REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
RELNAME = cds

.PHONY: all compile devrel start test clean distclean dialyze damsel

all: compile

compile: damsel
	$(REBAR) compile

damsel:
	git submodule update --init apps/cds/damsel

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
