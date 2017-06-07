-module(cds_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cds/src/cds_cds_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).
-export([lock/1]).
-export([unlock/1]).
-export([put_card_data/1]).
-export([get_card_data/1]).
-export([rotate/1]).
-export([recrypt/1]).
-export([session_cleaning/1]).
-export([refresh_sessions/1]).
-export([init_keyring_exists/1]).
-export([rotate_keyring_locked/1]).
-export([get_card_data_keyring_locked/1]).
-export([get_session_card_data_keyring_locked/1]).

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
    Storage = [
        {storage, cds_storage_riak},
        {cds_storage_riak, #{
            conn_params => #{
                host => "riakdb",
                port => 8087
            }
        }}
    ],
    [{storage_config, Storage} | C];

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
    _MasterKeys = cds_client:init(2, 3, root_url(C1)),
    ok = cds_client:lock(root_url(C1)),
    C1 ++ C;

init_per_group(session_management, C) ->
    CleanConfig = [
        {
            session_cleaning,
            #{
                session_lifetime => 4,
                batch_size => 1000,
                interval => 1000
            }
        }
    ],

    Recrypting = [
        {recrypting, #{
            interval => 2000
        }}
    ],
    C1 = [{recrypting_config, Recrypting}, {session_cleaning_config, CleanConfig} | C],
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
    {save_config, MasterKeys}.

-spec lock(config()) -> _.

lock(C) ->
    {init, MasterKeys} = config(saved_config, C),
    ok = cds_client:lock(root_url(C)),
    #'KeyringLocked'{} = (catch cds_client:put_card_data(?CREDIT_CARD(?CVV), root_url(C))),
    {save_config, MasterKeys}.

-spec unlock(config()) -> _.

unlock(C) ->
    {lock, [MasterKey1, MasterKey2, _MasterKey3]} = config(saved_config, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1, root_url(C)),
    {unlocked, #'Unlocked'{}} = cds_client:unlock(MasterKey2, root_url(C)),
    ok.

-spec put_card_data(config()) -> _.

put_card_data(C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        }
    } = cds_client:put_card_data(?CREDIT_CARD(?CVV), root_url(C)),
    {save_config, Token}.

-spec get_card_data(config()) -> _.

get_card_data(C) ->
    {put_card_data, Token} = config(saved_config, C),
    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(Token, root_url(C)),
    ok.

-spec rotate(config()) -> _.

rotate(C) ->
    ok = cds_client:rotate(root_url(C)),
    ok.

-spec init_keyring_exists(config()) -> _.

init_keyring_exists(C) ->
    #'KeyringExists'{} = (catch cds_client:init(2, 3, root_url(C))).

-spec rotate_keyring_locked(config()) -> _.

rotate_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate(root_url(C))).

-spec get_card_data_keyring_locked(config()) -> _.

get_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get_card_data(<<"No matter what">>, root_url(C))).

-spec get_session_card_data_keyring_locked(config()) -> _.

get_session_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get_session_card_data(
        <<"No matter what">>,
        <<"No matter what">>,
        root_url(C))
    ).

-spec session_cleaning(config()) -> _.

session_cleaning(C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        },
        session = Session
    } = cds_client:put_card_data(?CREDIT_CARD(?CVV), root_url(C)),

    ?CREDIT_CARD(<<>>) = cds_client:get_card_data(Token, root_url(C)),
    ?CREDIT_CARD(?CVV) = cds_client:get_session_card_data(Token, Session, root_url(C)),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),
    timer:sleep((Lifetime + 1) * 1000 + Interval),
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
        bank_card = #'BankCard'{
            token = Token
        },
        session = Session
    } = cds_client:put_card_data(?CREDIT_CARD(<<"345">>), root_url(C)),

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

%% dishonest test which uses internal functions
recrypt(C) ->
    {KeyID0, _} = cds_keyring_manager:get_current_key(),
    CardData = ?CREDIT_CARD(<<"345">>),
    {Token, Session} = cds:put_card_data(
        cds_card_data:unique(CardData),
        cds_card_data:marshall(CardData)
    ),
    {EncryptedCardData0, EncryptedCvv0} = cds_storage:get_session_card_data(Token, Session),
    <<KeyID0, _/binary>> = EncryptedCardData0,
    <<KeyID0, _/binary>> = EncryptedCvv0,
    _ = cds_keyring_manager:rotate(),
    [{session_cleaning, #{
        interval := Interval
    }}] = config(session_cleaning_config, C),
    timer:sleep(Interval + 1000),
    {KeyID, _} = cds_keyring_manager:get_current_key(),
    true = (KeyID0 =/= KeyID),
    {EncryptedCardData, EncryptedCvv} = cds_storage:get_session_card_data(Token, Session),
    <<KeyID, _/binary>> = EncryptedCardData,
    <<KeyID, _/binary>> = EncryptedCvv.

%%
%% helpers
%%

start_clear(Config) ->
    IP = "::1",
    Port = 8022,
    RootUrl = "http://[" ++ IP ++ "]:" ++ integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    CdsEnv = [
            {ip, IP},
            {port, Port},
            {keyring_storage, cds_keyring_storage_env}
    ] ++ StorageConfig ++ CleanConfig ++ Recrypting,
    ok = clean_storage(CdsEnv),

    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {crash_log, false},
            {handlers, [
                {lager_common_test_backend, debug}
            ]}
        ]) ++
        genlib_app:start_application_with(cds, CdsEnv),
    [{apps, Apps}, {root_url, genlib:to_binary(RootUrl)}].

stop_clear(C) ->
    _ = (catch cds_keyring_storage_env:delete()),
    [ok = application:stop(App) || App <- config(apps, C)],
    C.

config(Key, Config) -> config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
    {value, {Key, Val}} ->
        Val;
    _ ->
        Default
    end.

root_url(C) -> config(root_url, C).


clean_storage(CdsEnv) ->
    case genlib_opts:get(storage, CdsEnv) of
        cds_storage_riak -> clean_riak_storage(CdsEnv);
        _ ->
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
