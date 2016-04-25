PROJECT = cds
PROJECT_DESCRIPTION = RBK Money Card Data Storage
PROJECT_VERSION = 0.0.1

DEPS = pooler scrypt shamir riakc woody lager
dep_scrypt = git https://github.com/kittee/erlang-scrypt
dep_shamir = git https://github.com/kittee/shamir
dep_woody = git https://github.com/rbkmoney/woody_erlang
LOCAL_DEPS = crypto

IGNORE_DEPS += proper

SHELL_DEPS = sync
SHELL_OPTS = -s sync -s cds -config rel/sys.config

TEST_DEPS = meck proper
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

BUILD_DEPS = elvis_mk
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

DEP_PLUGINS = elvis_mk

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

# TODO: make thrift plugin
$(PROJECT).d:: src/cds_interface_*

src/cds_interface_*:: thrift/interface.thrift
	$(gen_verbose) thrift --gen erlang:idiomatic --out src thrift/interface.thrift

.PHONY: clean-thrift

clean:: clean-thrift

clean-thrift:
	$(gen_verbose) rm -rf src/cds_interface_*
