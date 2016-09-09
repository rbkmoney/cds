-module(cds_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("cds/src/cds_cds_thrift.hrl").
-compile(export_all).

-define(CVV, <<"777">>).
-define(CREDIT_CARD(CVV), #'CardData'{
    pan = <<"1234123412341234">>,
    exp_date = #'ExpDate'{
        month = 12,
        year = 2019
    },
    cardholder_name = <<"Tony Stark">>, %% temporarily hardcoded instead of saved
    cvv = CVV
}).
%%
%% tests descriptions
%%
all() ->
    [
        {group, basic_lifecycle},
        {group, keyring_errors}
    ].

groups() ->
    [
        {basic_lifecycle, [sequence], [
            init,
            lock,
            unlock,
            put,
            get,
            rotate
        ]},
        {keyring_errors, [parallel], [
            init_keyring_exists,
            rotate_keyring_locked,
            get_card_data_keyring_locked,
            get_session_card_data_keyring_locked
        ]}
    ].
%%
%% starting/stopping
%%
application_stop(App=sasl) ->
    %% hack for preventing sasl deadlock
    %% http://erlang.org/pipermail/erlang-questions/2014-May/079012.html
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(App),
    error_logger:add_report_handler(cth_log_redirect),
    ok;
application_stop(App) ->
    application:stop(App).

init_per_group(keyring_errors, C) ->
    {ok, Apps} = clear_start(),
    cds_client:init(2,3),
    ok = cds_client:lock(),
    [{apps, Apps} | C];

init_per_group(_, C) ->
    {ok, Apps} = clear_start(),
    [{apps, Apps} | C].

end_per_group(_, C) ->
    cds_keyring_storage_env:delete(),
    [application_stop(App) || App <- proplists:get_value(apps, C)].

%%
%% tests
%%
init(_C) ->
    MasterKeys = cds_client:init(2,3),
    3 = length(MasterKeys),
    {save_config, MasterKeys}.

lock(C) ->
    {init, MasterKeys} = ?config(saved_config, C),
    ok = cds_client:lock(),
    #'KeyringLocked'{} = (catch cds_client:put(?CREDIT_CARD(?CVV))),
    {save_config, MasterKeys}.

unlock(C) ->
    {lock, [MasterKey1, MasterKey2, _MasterKey3]} = ?config(saved_config, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1),
    {ok, #'Ok'{}} = cds_client:unlock(MasterKey2),
    ok.

put(_C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        }
    } = cds_client:put(?CREDIT_CARD(?CVV)),
    {save_config, Token}.

get(C) ->
    {put, Token} = ?config(saved_config, C),
    ?CREDIT_CARD(<<>>) = cds_client:get(Token),
    ok.

rotate(_C) ->
    ok = cds_client:rotate(),
    ok.

init_keyring_exists(_C) ->
    #'KeyringExists'{} = (catch cds_client:init(2,3)).

rotate_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate()).

get_card_data_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:get(<<"No matter what">>)).

get_session_card_data_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:get_session(<<"No matter what">>, <<"No matter what">>)).

%%
%% helpers
%%

clear_start() ->
    test_configuration(),
    application:ensure_all_started(cds).

test_configuration() ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_env),
    application:set_env(cds, storage, cds_storage_ets).

