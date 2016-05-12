-module(cds_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("cds/include/cds_thrift.hrl").
-compile(export_all).

%%
%% tests descriptions
%%
all() ->
    [
        unlock_keyring,
        basic_crud,
        basic_thrift
    ].

%%
%% starting/stopping
%%
init_per_suite(C) ->
    test_configuration(),
    {ok, Apps} = application:ensure_all_started(cds),
    [{apps, Apps}|C].

end_per_suite(C) ->
    [application_stop(App) || App <- proplists:get_value(apps, C)].

application_stop(App=sasl) ->
    %% hack for preventing sasl deadlock
    %% http://erlang.org/pipermail/erlang-questions/2014-May/079012.html
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(App),
    error_logger:add_report_handler(cth_log_redirect),
    ok;
application_stop(App) ->
    application:stop(App).

%%
%% tests
%%
unlock_keyring(_C) ->
    [MasterOne, MasterTwo, _MasterThree, MasterFour] = prepare_keyring(3, 4),
    {more, 2} = cds:unlock_keyring(MasterOne),
    {more, 1} = cds:unlock_keyring(MasterTwo),
    unlocked = cds:unlock_keyring(MasterFour),
    ok.

basic_crud(C) ->
    ok = unlock_keyring(C),
    CreditCard = <<"1234-5678-8765-4321 10/20">>,
    Token = cds:put(CreditCard),
    Token = cds:put(CreditCard),
    CreditCard = cds:get(Token),
    ok = cds:delete(Token),
    not_found = (catch cds:get(Token)),
    ok.

basic_thrift(_C) ->
    [MasterKey] = prepare_keyring(1,1),
    {{ok, #unlock_status{unlocked = true}}, _} = woody_call(unlock, [MasterKey]),
    CreditCard = <<"1234-5678-8765-4321 10/20">>,
    {{ok, Token}, _} = woody_call(put_card_data, [CreditCard]),
    {{ok, CreditCard}, _} = woody_call(get_card_data, [Token]),
    ok.

%%
%% helpers
%%

woody_random_context() ->
    woody_client:new_context(crypto:rand_bytes(10), cds_thrift_event_handler).

woody_call(Function, Args) ->
    woody_client:call(woody_random_context(), {{cds_thrift, cds}, Function, Args}, #{url => "localhost:8022/v1/cds"}).

test_configuration() ->
    application:set_env(cds, riak_host, "192.168.99.100"),
    application:set_env(cds, riak_port, 32777),
    application:set_env(cds, scrypt_opts, {16384, 8, 1}),
    application:set_env(cds, keyring_storage, cds_keyring_file_storage),
    application:set_env(cds, keyring_location, "/tmp/cdskeyring"),
    application:set_env(cds, keyring_lock_location, "/tmp/cdslock"),
    application:set_env(cds, storage, cds_riak_storage),
    application:set_env(pooler, pools, [
        [
            {name, riak1},
            {group, riak},
            {max_count, 5},
            {init_count, 2},
            {start_mfa, {riakc_pb_socket, start_link, ["192.168.99.100", 32769]}}
        ],
        [
            {name, riak2},
            {group, riak},
            {max_count, 5},
            {init_count, 2},
            {start_mfa, {riakc_pb_socket, start_link, ["192.168.99.100", 32771]}}
        ],
        [
            {name, riak3},
            {group, riak},
            {max_count, 5},
            {init_count, 2},
            {start_mfa, {riakc_pb_socket, start_link, ["192.168.99.100", 32773]}}
        ],
        [
            {name, riak4},
            {group, riak},
            {max_count, 5},
            {init_count, 2},
            {start_mfa, {riakc_pb_socket, start_link, ["192.168.99.100", 32775]}}
        ],
        [
            {name, riak5},
            {group, riak},
            {max_count, 5},
            {init_count, 2},
            {start_mfa, {riakc_pb_socket, start_link, ["192.168.99.100", 32777]}}
        ]
    ]).

prepare_keyring(Threshold, Shares) ->
    ok = cds:destroy_keyring(),
    Keys = cds:init_keyring(Threshold, Shares),
    ok = cds:update_keyring(),
    Keys.

