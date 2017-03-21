-module(cds_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cds/src/cds_cds_thrift.hrl").
-compile(export_all).

-define(config(K, C), begin element(2, lists:keyfind(K, 1, C)) end).
-define(root_url(C), ?config(root_url, C)).

%%

-define(CVV, <<"777">>).
-define(CREDIT_CARD(CVV), #'CardData'{
    pan = <<"5321301234567892">>,
    exp_date = #'ExpDate'{
        month = 12,
        year = 3000
    },
    cardholder_name = <<"Tony Stark">>, %% temporarily hardcoded instead of saved
    cvv = CVV
}).


%%
%% tests descriptions
%%
all() ->
    [
        {group, riak_storage_backend},
        {group, ets_storage_backend}
    ].

groups() ->
 [
        {riak_storage_backend, [], [{group, general_flow}]},
        {ets_storage_backend, [], [{group, general_flow}]},
        {general_flow, [], [
            {group, basic_lifecycle},
            {group, keyring_errors},
            {group, session_management}
        ]},
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
        ]},
        {session_management, [sequence], [
            init,
            lock,
            unlock,
            session_cleaning
        ]}
    ].
%%
%% starting/stopping
%%
init_per_suite(C) ->
    timer:sleep(10000), %% sleep again ;(
    C.

init_per_group(riak_storage_backend, C) ->
    Storage = [
        {storage, cds_storage_riak},
        {cds_storage_riak, #{
            conn_params => {"riakdb", 8087}
        }}
    ],
    [{storage_config, Storage} | C];

init_per_group(ets_storage_backend, C) ->
    Storage = [
        {storage, cds_storage_ets}
    ],
    [{storage_config, Storage} | C];

init_per_group(general_flow, C) ->
    C;

init_per_group(keyring_errors, C) ->
    C1 = start_clear(?config(storage_config, C)),
    _MasterKeys = cds_client:init(2, 3, ?root_url(C1)),
    ok = cds_client:lock(?root_url(C1)),
    C1 ++ C;

init_per_group(session_management, C) ->
    _ = application:load(cds),
    CleanerConf = genlib_app:env(cds, session_cleaner, #{}),
    application:set_env(
        cds,
        session_cleaner,
        CleanerConf#{
            session_lifetime => 3,
            timeout => 1000
        }
    ),
    C1 = [{session_cleaner_config, CleanerConf} | C],
    C1 ++ start_clear(?config(storage_config, C1)) ++ C;

init_per_group(_, C) ->
    C1 = start_clear(?config(storage_config, C)),
    C1 ++ C.

end_per_group(session_management, C) ->
    application:set_env(
        cds,
        session_cleaner,
        ?config(session_cleaner_config, C)
    ),
    stop_clear(C);

end_per_group(Group, C) when
    Group =:= ets_storage_backend;
    Group =:= riak_storage_backend;
    Group =:= general_flow
 ->
    C;

end_per_group(_, C) ->
    stop_clear(C).

%%
%% tests
%%
init(C) ->
    MasterKeys = cds_client:init(2, 3, ?root_url(C)),
    3 = length(MasterKeys),
    {save_config, MasterKeys}.

lock(C) ->
    {init, MasterKeys} = ?config(saved_config, C),
    ok = cds_client:lock(?root_url(C)),
    #'KeyringLocked'{} = (catch cds_client:put(?CREDIT_CARD(?CVV), ?root_url(C))),
    {save_config, MasterKeys}.

unlock(C) ->
    {lock, [MasterKey1, MasterKey2, _MasterKey3]} = ?config(saved_config, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1, ?root_url(C)),
    {unlocked, #'Unlocked'{}} = cds_client:unlock(MasterKey2, ?root_url(C)),
    ok.

put(C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        }
    } = cds_client:put(?CREDIT_CARD(?CVV), ?root_url(C)),
    {save_config, Token}.

get(C) ->
    {put, Token} = ?config(saved_config, C),
    ?CREDIT_CARD(<<>>) = cds_client:get(Token, ?root_url(C)),
    ok.

rotate(C) ->
    ok = cds_client:rotate(?root_url(C)),
    ok.

init_keyring_exists(C) ->
    #'KeyringExists'{} = (catch cds_client:init(2, 3, ?root_url(C))).

rotate_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate(?root_url(C))).

get_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get(<<"No matter what">>, ?root_url(C))).

get_session_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get_session(
        <<"No matter what">>,
        <<"No matter what">>,
        ?root_url(C))
    ).

session_cleaning(C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        },
        session = Session
    } = cds_client:put(?CREDIT_CARD(?CVV), ?root_url(C)),

    ?CREDIT_CARD(<<>>) = cds_client:get(Token, ?root_url(C)),
    ?CREDIT_CARD(?CVV) = cds_client:get_session(Token, Session, ?root_url(C)),

    #{
        session_lifetime := Lifetime,
        timeout := Timeout
    } = genlib_app:env(cds, session_cleaner),
    timer:sleep((Lifetime + 1) * 1000 + Timeout),
    ok = try
        _ = cds_client:get_session(Token, Session, ?root_url(C)),
        error
    catch
        throw:#'CardDataNotFound'{} ->
          ok
    end,
    ?CREDIT_CARD(<<>>) = cds_client:get(Token, ?root_url(C)).

%%
%% helpers
%%

start_clear(Storage) ->
    IP = "::1",
    Port = 8022,
    RootUrl = "http://[" ++ IP ++ "]:" ++ integer_to_list(Port),
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {crash_log, false},
            {handlers, [
                {lager_common_test_backend, [debug, true]}
            ]}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, "::1"},
            {port, 8022},
            {keyring_storage, cds_keyring_storage_env}
        ] ++ Storage),
    [{apps, Apps}, {root_url, genlib:to_binary(RootUrl)}].

stop_clear(C) ->
    _ = (catch cds_keyring_storage_env:delete()),
    [ok = application:stop(App) || App <- ?config(apps, C)],
    C.
