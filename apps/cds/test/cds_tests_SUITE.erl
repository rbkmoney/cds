-module(cds_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("cds/include/cds_thrift.hrl").
-compile(export_all).

-define(CREDIT_CARD, <<"1234-1234-1234-1234 12/18">>).
%%
%% tests descriptions
%%
all() ->
    [
        {group, basic_lifecycle}
    ].

groups() ->
    [
        {basic_lifecycle, [sequence], [
            init,
            unlock,
            put,
            get,
            rotate,
            lock
        ]}
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
init(_C) ->
    MasterKeys = cds_client:init(2,3),
    3 = length(MasterKeys),
    {save_config, MasterKeys}.

unlock(C) ->
    {init, [MasterKey1, MasterKey2, _MasterKey3]} = ?config(saved_config, C),
    #unlock_status{unlocked = false, more_keys_needed = 1} = cds_client:unlock(MasterKey1),
    #unlock_status{unlocked = true} = cds_client:unlock(MasterKey2),
    ok.

put(_C) ->
    Token = cds_client:put(?CREDIT_CARD),
    {save_config, Token}.

get(C) ->
    {put, Token} = ?config(saved_config, C),
    ?CREDIT_CARD = cds_client:get(Token),
    ok.

rotate(_C) ->
    ok = cds_client:rotate(),
    ok.

lock(_C) ->
    ok = cds_client:lock(),
    {locked} = (catch cds_client:put(?CREDIT_CARD)),
    ok.


%%
%% helpers
%%

test_configuration() ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_env),
    application:set_env(cds, storage, cds_storage_ets).

