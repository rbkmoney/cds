-module(cds_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).
-export([lock/1]).
-export([unlock/1]).
-export([put_card_data/1]).
-export([get_card_data/1]).
-export([get_session_data/1]).
-export([put_card_data_3ds/1]).
-export([get_card_data_3ds/1]).
-export([get_session_data_3ds/1]).
-export([rotate/1]).
-export([recrypt/1]).
-export([session_cleaning/1]).
-export([refresh_sessions/1]).
-export([init_keyring_exists/1]).
-export([lock_no_keyring/1]).
-export([rotate_keyring_locked/1]).
-export([put_card_data_unavailable/1]).
-export([put_card_data_3ds_unavailable/1]).
-export([get_card_data_unavailable/1]).
-export([get_session_card_data_unavailable/1]).

%%

-define(CVV, <<"777">>).
-define(CARD_SEC_CODE(Value), {card_security_code, #'CardSecurityCode'{
    value = Value
}}).

-define(AUTH_3DS, {auth_3ds, #'Auth3DS'{
    cryptogram = <<"somecryptogram">>,
    eci = <<"5">>
}}).

-define(SESSION_DATA(AuthData), #'SessionData'{
    auth_data = AuthData
}).

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

-type config() :: term().

-spec all() -> [{group, atom()}].

all() ->
    [
        {group, riak_storage_backend},
        {group, ets_storage_backend},
        {group, keyring_errors}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].

groups() ->
 [
        {riak_storage_backend, [], [{group, general_flow}]},
        {ets_storage_backend, [], [{group, general_flow}]},
        {general_flow, [], [
            {group, basic_lifecycle},
            {group, session_management}
        ]},
        {basic_lifecycle, [sequence], [
            init,
            lock,
            unlock,
            put_card_data,
            get_card_data,
            get_session_data,
            rotate,
            put_card_data_3ds,
            get_card_data_3ds,
            get_session_data_3ds,
            rotate
        ]},
        {keyring_errors, [sequence], [
            get_card_data_unavailable,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            lock_no_keyring,
            init,
            init_keyring_exists,
            lock,
            lock,
            init_keyring_exists,
            rotate_keyring_locked,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            get_card_data_unavailable,
            get_session_card_data_unavailable
        ]},
        {session_management, [sequence], [
            init,
            lock,
            unlock,
            session_cleaning,
            refresh_sessions,
            recrypt
        ]}
    ].
%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(riak_storage_backend, C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({cds_storage_riak, '_', '_'}, x),
    StorageConfig = [
        {storage, cds_storage_riak},
        {cds_storage_riak, #{
            conn_params => #{
                host => "riakdb",
                port => 8087
            },
            timeout => 5000
        }}
    ],
    [{storage_config, StorageConfig} | C];

init_per_group(ets_storage_backend, C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    [{storage_config, StorageConfig} | C];

init_per_group(general_flow, C) ->
    C;

init_per_group(keyring_errors, C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    C1 = start_clear([{storage_config, StorageConfig} | C]),
    C1 ++ C;

init_per_group(session_management, C) ->
    CleanerConfig = [
        {
            session_cleaning,
            #{
                session_lifetime => 6,
                batch_size => 1000,
                interval => 5000
            }
        }
    ],
    Recrypting = [
        {recrypting, #{
            interval => 1000
        }}
    ],
    C1 = [{recrypting_config, Recrypting}, {session_cleaning_config, CleanerConfig} | C],
    C2 = start_clear(C1),
    C1 ++ C2;

init_per_group(_, C) ->
    C1 = start_clear(C),
    C1 ++ C.

-spec end_per_group(atom(), config()) -> _.

end_per_group(Group, C) when
    Group =:= ets_storage_backend;
    Group =:= general_flow;
    Group =:= riak_storage_backend
 ->
    C;

end_per_group(_, C) ->
    stop_clear(C).

%%
%% tests
%%

-spec init(config()) -> _.

init(C) ->
    MasterKeys = cds_client:init(2, 3, root_url(C)),
    3 = length(MasterKeys),
    store(master_keys, MasterKeys, C).

-spec lock(config()) -> _.

lock(C) ->
    ok = cds_client:lock(root_url(C)).

-spec unlock(config()) -> _.

unlock(C) ->
    [MasterKey1, MasterKey2, _MasterKey3] = lookup(master_keys, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1, root_url(C)),
    {unlocked, #'Unlocked'{}} = cds_client:unlock(MasterKey2, root_url(C)).

-spec put_card_data(config()) -> _.

put_card_data(C) ->
    % check without cardholder
    CardData = #'CardData'{
        pan = <<"4242424242424242">>,
        exp_date = #'ExpDate'{
            month = 12,
            year = 3000
        }
    },
    #'PutCardDataResult'{} = cds_client:put_card_data(CardData, ?SESSION_DATA(?CARD_SEC_CODE(<<"123">>)), root_url(C)),
    % check with cardholder
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)),
    store([{token, Token}, {session, Session}], C).

-spec get_card_data(config()) -> _.

get_card_data(C) ->
    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(lookup(token, C), root_url(C)).

-spec get_session_data(config()) -> _.

get_session_data(C) ->
    ?SESSION_DATA(?CARD_SEC_CODE(?CVV)) = cds_client:get_session_data(lookup(session, C), root_url(C)).

-spec put_card_data_3ds(config()) -> _.

put_card_data_3ds(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?AUTH_3DS), root_url(C)),
    store([{token, Token}, {session, Session}], C).

-spec get_card_data_3ds(config()) -> _.

get_card_data_3ds(C) ->
    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(lookup(token, C), root_url(C)).

-spec get_session_data_3ds(config()) -> _.

get_session_data_3ds(C) ->
    ?SESSION_DATA(?AUTH_3DS) = cds_client:get_session_data(lookup(session, C), root_url(C)).

-spec rotate(config()) -> _.

rotate(C) ->
    ok = cds_client:rotate(root_url(C)).

-spec init_keyring_exists(config()) -> _.

init_keyring_exists(C) ->
    #'KeyringExists'{} = (catch cds_client:init(2, 3, root_url(C))).

-spec lock_no_keyring(config()) -> _.

lock_no_keyring(C) ->
    #'NoKeyring'{} = (catch cds_client:lock(root_url(C))).

-spec rotate_keyring_locked(config()) -> _.

rotate_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate(root_url(C))).

-spec get_card_data_unavailable(config()) -> _.

get_card_data_unavailable(C) ->
    try cds_client:get_card_data(<<"No matter what">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec get_session_card_data_unavailable(config()) -> _.

get_session_card_data_unavailable(C) ->
    try cds_client:get_session_card_data(<<"TOKEN">>, <<"SESSION">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec put_card_data_unavailable(config()) -> _.

put_card_data_unavailable(C) ->
    try cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec put_card_data_3ds_unavailable(config()) -> _.

put_card_data_3ds_unavailable(C) ->
    try cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?AUTH_3DS), root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec session_cleaning(config()) -> _.

session_cleaning(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)),

    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(Token, root_url(C)),
    ?CREDIT_CARD(?CVV) = cds_client:get_session_card_data(Token, Session, root_url(C)),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),
    timer:sleep(Lifetime*1000 + Interval*2),
    ok = try
        _ = cds_client:get_session_card_data(Token, Session, root_url(C)),
        error
    catch
        throw:#'CardDataNotFound'{} ->
            ok
    end,
    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(Token, root_url(C)).

-spec refresh_sessions(config()) -> _.

refresh_sessions(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(<<"345">>)), root_url(C)),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),

    [
        begin
            ok = cds_maintenance:refresh_sessions_created_at(),
            timer:sleep(Lifetime * 100)
        end
    || _ <- lists:seq(1, 25)],

    timer:sleep(Interval),

    _ = cds_client:get_session_card_data(Token, Session, root_url(C)),

    timer:sleep(Lifetime * 1000 + Interval),

    ok = try
        _ = cds_client:get_session_card_data(Token, Session, root_url(C)),
        error
    catch
        throw:#'CardDataNotFound'{} ->
            ok
    end,
    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(Token, root_url(C)).


-spec recrypt(config()) -> _.

%% dishonest test which uses external functions
recrypt(C) ->
    {KeyID0, _} = cds_keyring_manager:get_current_key(),
    CardholderData = #{
        cardnumber => <<"5321301234567892">>,
        exp_date => {12, 3000},
        cardholder => <<"Tony Stark">>
    },
    SessionDataCVV = #{auth_data => #{cvv => <<"345">>}},
    {TokenCVV, SessionCVV} = cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionDataCVV)
    }),

    SessionData3DS = #{auth_data => #{cryptogram => <<"cryptogram">>, eci => <<"5">>}},
    {Token3DS, Session3DS} = cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionData3DS)
    }),

    {EncryptedCardDataCVV0, EncryptedSessionDataCVV0} = cds_storage:get_session_card_data(TokenCVV, SessionCVV),
    <<KeyID0, _/binary>> = EncryptedCardDataCVV0,
    {arr,[{i,2},{obj,#{{bin,<<"auth_data">>}:={obj,#{{bin,<<"cvv">>}:={bin,<<KeyID0, _/binary>>}}}}}]} =
        EncryptedSessionDataCVV0,
    _ = cds_keyring_manager:rotate(),
    [{recrypting, #{
        interval := Interval
    }}] = config(recrypting_config, C),

    {EncryptedCardData3DS0, EncryptedSessionData3DS0} = cds_storage:get_session_card_data(Token3DS, Session3DS),
    <<KeyID0, _/binary>> = EncryptedCardData3DS0,
    {arr,[{i,2},{obj,#{
        {bin,<<"auth_data">>}:={obj,#{
            {bin,<<"cryptogram">>}:={bin,<<KeyID0, _/binary>>},
            {bin,<<"eci">>}:={bin,<<KeyID0, _/binary>>}
        }}
    }}]} = EncryptedSessionData3DS0,
    _ = cds_keyring_manager:rotate(),
    [{recrypting, #{
        interval := Interval
    }}] = config(recrypting_config, C),

    % we should meet reencryption at least once _after_ rotation
    _ = timer:sleep(Interval * 3),
    {KeyID, _} = cds_keyring_manager:get_current_key(),
    true = (KeyID0 =/= KeyID),
    {EncryptedCardDataCVV, EncryptedSessionDataCVV} = cds_storage:get_session_card_data(TokenCVV, SessionCVV),
    <<KeyID, _/binary>> = EncryptedCardDataCVV,
    {arr,[{i,2},{obj,#{{bin,<<"auth_data">>}:={obj,#{{bin,<<"cvv">>}:={bin,<<KeyID, _/binary>>}}}}}]} =
        EncryptedSessionDataCVV,

    {EncryptedCardData3DS, EncryptedSessionData3DS} = cds_storage:get_session_card_data(Token3DS, Session3DS),
    <<KeyID, _/binary>> = EncryptedCardData3DS,
    {arr,[{i,2},{obj,#{
        {bin,<<"auth_data">>}:={obj,#{
            {bin,<<"cryptogram">>}:={bin,<<KeyID, _/binary>>},
            {bin,<<"eci">>}:={bin,<<KeyID, _/binary>>}
        }}
    }}]} = EncryptedSessionData3DS.

%%
%% helpers
%%

start_clear(Config) ->
    IP = "127.0.0.1",
    Port = 8022,
    RootUrl = "http://" ++ IP ++ ":" ++ integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    ok = clean_storage(StorageConfig),
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {crash_log, false},
            {handlers, [
                {lager_common_test_backend, warning}
            ]}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, IP},
            {port, Port},
            {keyring_storage, cds_keyring_storage_env},
            {net_opts, [
                % Bump keepalive timeout up to a minute
                {timeout, 60000}
            ]}
        ] ++ StorageConfig ++ CleanConfig ++ Recrypting)
    ,
    start_stash([
        {apps, lists:reverse(Apps)},
        {root_url, genlib:to_binary(RootUrl)}
    ]).

stop_clear(C) ->
    _ = (catch cds_keyring_storage_env:delete()),
    [ok = application:stop(App) || App <- config(apps, C)],
    stop_stash(C).

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

root_url(C) ->
    config(root_url, C).

clean_storage(CdsEnv) ->
    case genlib_opts:get(storage, CdsEnv) of
        cds_storage_riak ->
            clean_riak_storage(CdsEnv);
        cds_storage_ets ->
            ok
    end.

clean_riak_storage(CdsEnv) ->
    _ = application:start(riakc),
    _ = application:set_env(riakc, allow_listing, true),
    #{conn_params := #{
        host := Host,
        port := Port
    }} = genlib_opts:get(cds_storage_riak, CdsEnv),
    {ok, Client} = riakc_pb_socket:start_link(Host, Port),
    {ok, Buckets} = riakc_pb_socket:list_buckets(Client),
    lists:foreach(
        fun(B) ->
            {ok, Keys} = riakc_pb_socket:list_keys(Client, B),
            [
                ok = riakc_pb_socket:delete(Client, B, K)
                    || K <- Keys
            ]
        end,
        Buckets
    ),
    ok.

%%

start_stash(C) ->
    [{stash, cds_ct_stash:start()} | C].

stop_stash(C) ->
    cds_ct_stash:stop(config(stash, C)).

store(KVs, C) when is_list(KVs) ->
    [store(Key, Value, C) || {Key, Value} <- KVs].

store(Key, Value, C) ->
    cds_ct_stash:put(config(stash, C), Key, Value).

lookup(Key, C) ->
    cds_ct_stash:get(config(stash, C), Key).
