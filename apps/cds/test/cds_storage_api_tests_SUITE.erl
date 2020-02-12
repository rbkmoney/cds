-module(cds_storage_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).
-export([lock/1]).
-export([unlock/1]).
-export([rekey/1]).
-export([rotate/1]).
-export([put_card_data/1]).
-export([get_card_data/1]).
-export([get_session_data/1]).
-export([put_card/1]).
-export([put_session/1]).
-export([put_card_data_3ds/1]).
-export([get_card_data_3ds/1]).
-export([get_session_data_3ds/1]).
-export([get_session_data_backward_compatibility/1]).
-export([get_session_card_data_backward_compatibility/1]).
-export([get_card_data_backward_compatibility/1]).
-export([no_last_dot_in_token/1]).
-export([recrypt/1]).
-export([session_cleaning/1]).
-export([refresh_sessions/1]).
-export([put_card_data_unavailable/1]).
-export([put_card_data_3ds_unavailable/1]).
-export([get_card_data_unavailable/1]).
-export([get_session_card_data_unavailable/1]).
-export([put_card_data_no_member/1]).
-export([same_card_data_has_same_token/1]).
-export([same_card_number_has_same_token/1]).

%%

-define(CVV, <<"777">>).
-define(CARD_SEC_CODE(Value), {card_security_code, #{
    value => Value
}}).

-define(CARD_SEC_CODE_MATCH(Value), {card_security_code, #{
    value := Value
}}).

-define(AUTH_3DS, {auth_3ds, #{
    cryptogram => <<"somecryptogram">>,
    eci => <<"5">>
}}).

-define(AUTH_3DS_MATCH, {auth_3ds, #{
    cryptogram := <<"somecryptogram">>,
    eci := <<"5">>
}}).

-define(SESSION_DATA(AuthData), #{
    auth_data => AuthData
}).

-define(SESSION_DATA_MATCH(AuthData), #{
    auth_data := AuthData
}).

-define(ENCRYPT_VERSION, 16#80).

%%
%% tests descriptions
%%

-type config() :: term().

-spec test() -> _.

-spec all() -> [{group, atom()}].

all() ->
    [
        {group, cds_client_v1},
        {group, cds_client_v2}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].

groups() ->
    [
        {cds_client_v1, [], [
            {group, all_groups},
            {group, backward_compatibility}
        ]},
        {cds_client_v2, [], [
            {group, all_groups},
            {group, backward_compatibility}
        ]},
        {all_groups, [], [
            {group, riak_storage_backend},
            {group, ets_storage_backend},
            {group, keyring_errors},
            {group, token_check}
        ]},
        {riak_storage_backend, [], [
            {group, general_flow},
            {group, error_map}
        ]},
        {ets_storage_backend, [], [{group, general_flow}]},
        {backward_compatibility, [], [
            {riak_storage_backend, [], [
                {group, backward_compatibility_basic_lifecycle},
                {group, backward_compatibility_data_storage}
            ]},
            {ets_storage_backend, [], [{group, backward_compatibility_basic_lifecycle}]},
            {keyring_errors, [], [
                init,
                lock,
                get_session_card_data_unavailable,
                unlock
            ]}
        ]},
        {backward_compatibility_basic_lifecycle, [], [
            {general_flow, [], [
                {basic_lifecycle, [sequence], [
                    init,
                    put_card_data,
                    get_session_card_data_backward_compatibility
                ]}
            ]}
        ]},
        {backward_compatibility_data_storage, [], [
            get_card_data_backward_compatibility,
            no_last_dot_in_token
        ]},
        {general_flow, [], [
            {group, basic_lifecycle},
            {group, session_management}
        ]},
        {basic_lifecycle, [sequence], [
            init,
            put_card_data,
            get_card_data,
            lock,
            unlock,
            put_card_data,
            get_card_data,
            rekey,
            put_card_data,
            get_card_data,
            rotate,
            get_session_data,
            put_card,
            put_session,
            put_card_data_3ds,
            rotate,
            get_card_data_3ds,
            get_session_data_3ds,
            rotate,
            {group, hash_collision_check}
        ]},
        {keyring_errors, [sequence], [
            get_card_data_unavailable,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            init,
            lock,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            get_card_data_unavailable,
            unlock
        ]},
        {session_management, [sequence], [
            init,
            lock,
            unlock,
            session_cleaning,
            refresh_sessions,
            recrypt
        ]},
        {hash_collision_check, [parallel, {repeat, 2}], [
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            put_card_data,
            rotate
        ]},
        {error_map, [sequence], [
            init,
            {group, error_map_ddos}
        ]},
        {error_map_ddos, [parallel], [
            put_card_data_no_member,
            put_card_data_no_member
        ]},
        {token_check, [sequence], [
            init,
            same_card_number_has_same_token,
            same_card_data_has_same_token
        ]}
    ].
%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(cds_client_v1, C) ->
    [{cds_storage_client, cds_card_v1_client}] ++ C;

init_per_group(cds_client_v2, C) ->
    [{cds_storage_client, cds_card_v2_client}] ++ C;

init_per_group(riak_storage_backend, C) ->
    cds_ct_utils:set_riak_storage(C);

init_per_group(ets_storage_backend, C) ->
    cds_ct_utils:set_ets_storage(C);

init_per_group(all_groups, C) ->
    C;

init_per_group(backward_compatibility, C) ->
    C;

init_per_group(backward_compatibility_basic_lifecycle, C) ->
    C;

init_per_group(general_flow, C) ->
    C;
init_per_group(hash_collision_check, C) ->
    cds_ct_utils:start_clear(C);

init_per_group(keyring_errors, C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear([{storage_config, StorageConfig} | C]);

init_per_group(session_management, C) ->
    CleanerConfig = [
        {
            session_cleaning,
            #{
                enabled => true,
                session_lifetime => 5,
                batch_size => 1000,
                interval => 1000
            }
        }
    ],
    Recrypting = [
        {recrypting, #{
            enabled => true,
            interval => 1000
        }}
    ],
    C1 = [{recrypting_config, Recrypting}, {session_cleaning_config, CleanerConfig} | C],
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear(C1);

init_per_group(error_map, C) ->
    StorageConfig = config(storage_config, C),
    RiakConfig = config(cds_storage_riak, StorageConfig),
    RiakConfigNew = RiakConfig#{
        pool_params => #{
            max_count     => 1,
            init_count    => 1,
            cull_interval => {0, min},
            pool_timeout  => {0, sec}
        }
    },

    StorageConfigNew = update_config(cds_storage_riak, StorageConfig, RiakConfigNew),
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear([{storage_config, StorageConfigNew} | C]);
init_per_group(error_map_ddos, C) ->
    C;

init_per_group(token_check, C) ->
    C1 = cds_ct_utils:set_riak_storage(C),
    cds_ct_utils:start_clear(C1);

init_per_group(_, C) ->
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear(C).

-spec end_per_group(atom(), config()) -> _.

end_per_group(Group, C) when
    Group =:= ets_storage_backend;
    Group =:= general_flow;
    Group =:= riak_storage_backend;
    Group =:= error_map_ddos;
    Group =:= all_groups;
    Group =:= cds_client_v1;
    Group =:= cds_client_v2;
    Group =:= backward_compatibility;
    Group =:= backward_compatibility_basic_lifecycle
    ->
    C;

end_per_group(_, C) ->
    cds_ct_utils:stop_clear(C).

%%
%% tests
%%

-spec init(config()) -> _.

init(C) ->
    ok = cds_ct_keyring:ensure_init(C),
    ok = cds_ct_utils:wait_for_keyring(C).

-spec lock(config()) -> _.

lock(C) ->
    cds_ct_keyring:lock(C).

-spec unlock(config()) -> _.

unlock(C) ->
    cds_ct_keyring:unlock(C).

-spec rekey(config()) -> _.

rekey(C) ->
    cds_ct_keyring:rekey(C).

-spec rotate(config()) -> _.

rotate(C) ->
    cds_ct_keyring:rotate(C).

-spec put_card_data(config()) -> _.

put_card_data(C) ->
    CDSCardClient = config(cds_storage_client, C),
    % check without cardholder
    CardData = #{
        pan => <<"4242424242424242">>,
        exp_date => #{
            month => 12,
            year => 3000
        }
    },
    #{} = CDSCardClient:put_card_and_session(
        CardData,
        ?SESSION_DATA(?CARD_SEC_CODE(<<"123">>)),
        root_url(C)
    ),
    % check with cardholder
    #{
        bank_card := #{
            token := Token
        },
        session_id := Session
    } = CDSCardClient:put_card_and_session(
        CDSCardClient:get_test_card(undefined),
        ?SESSION_DATA(?CARD_SEC_CODE(?CVV)),
        root_url(C)
    ),
    cds_ct_utils:store([{token, Token}, {session, Session}]).

-spec get_card_data(config()) -> _.

get_card_data(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?assertEqual(
        CDSCardClient:get_test_card(<<>>),
        CDSCardClient:get_card_data(
            cds_ct_utils:lookup(token),
            root_url(C)
        )
    ).

-spec get_session_data(config()) -> _.

get_session_data(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?SESSION_DATA_MATCH(?CARD_SEC_CODE_MATCH(?CVV)) = CDSCardClient:get_session_data(
        cds_ct_utils:lookup(session),
        root_url(C)
    ).

-spec put_card(config()) -> _.

put_card(C) ->
    CDSCardClient = config(cds_storage_client, C),
    CardData = #{
        pan => <<"4242424242424648">>,
        exp_date => #{
            month => 11,
            year => 3000
        }
    },
    #{
        bank_card := #{
            token := Token
        }
    } = CDSCardClient:put_card(CardData, root_url(C)),
    CardData2 = CDSCardClient:get_test_card(CardData, <<>>),
    ?assertEqual(CardData2, CDSCardClient:get_card_data(Token, root_url(C))).

-spec same_card_data_has_same_token(config()) -> _.

same_card_data_has_same_token(C) ->
    CDSCardClient = config(cds_storage_client, C),
    CardData = #{
        pan => <<"4242424242424648">>,
        exp_date => #{
            month => 11,
            year => 3000
        }
    },
    #{
        bank_card := #{
            token := Token1
        }
    } = CDSCardClient:put_card(CardData, root_url(C)),
    #{
        bank_card := #{
            token := Token2
        }
    } = CDSCardClient:put_card(CardData, root_url(C)),
    ?assertEqual(Token1, Token2).

-spec same_card_number_has_same_token(config()) -> _.

same_card_number_has_same_token(C) ->
    CDSCardClient = config(cds_storage_client, C),
    CardData = #{
        pan => <<"4242424242424648">>,
        exp_date => #{
            month => 11,
            year => 3000
        }
    },
    #{
        bank_card := #{
            token := Token1
        }
    } = CDSCardClient:put_card(CardData, root_url(C)),
    #{
        bank_card := #{
            token := Token2
        }
    } = CDSCardClient:put_card(CardData#{cardholder_name => <<"Tony Stark">>}, root_url(C)),
    {CardDataToken1, _} = cds_utils:decode_token_with_payload(Token1),
    {CardDataToken2, _} = cds_utils:decode_token_with_payload(Token2),

    ?assertEqual(CardDataToken1, CardDataToken2).

-spec put_session(config()) -> _.

put_session(C) ->
    CDSCardClient = config(cds_storage_client, C),
    SessionID = crypto:strong_rand_bytes(16),
    SessionData = ?SESSION_DATA(?CARD_SEC_CODE(?CVV)),
    ok = CDSCardClient:put_session(SessionID, SessionData, root_url(C)),
    SessionData = CDSCardClient:get_session_data(SessionID, root_url(C)).

-spec put_card_data_3ds(config()) -> _.

put_card_data_3ds(C) ->
    CDSCardClient = config(cds_storage_client, C),
    #{
        bank_card := #{
            token := Token
        },
        session_id := Session
    } = CDSCardClient:put_card_and_session(
        CDSCardClient:get_test_card(undefined),
        ?SESSION_DATA(?AUTH_3DS),
        root_url(C)
    ),
    cds_ct_utils:store([{token, Token}, {session, Session}]).

-spec get_card_data_3ds(config()) -> _.

get_card_data_3ds(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?assertEqual(
        CDSCardClient:get_test_card(<<>>),
        CDSCardClient:get_card_data(cds_ct_utils:lookup(token), root_url(C))
    ).

-spec get_session_data_3ds(config()) -> _.

get_session_data_3ds(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?SESSION_DATA_MATCH(?AUTH_3DS_MATCH) = CDSCardClient:get_session_data(cds_ct_utils:lookup(session), root_url(C)).

-spec get_session_data_backward_compatibility(config()) -> _.

get_session_data_backward_compatibility(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?SESSION_DATA_MATCH(?CARD_SEC_CODE_MATCH(?CVV)) = CDSCardClient:get_session_data(
        cds_ct_utils:lookup(session),
        root_url(C)
    ).

-spec get_session_card_data_backward_compatibility(config()) -> _.

get_session_card_data_backward_compatibility(C) ->
    CDSCardClient = config(cds_storage_client, C),
    ?assertEqual(
        CDSCardClient:get_test_card(?CVV),
        CDSCardClient:get_session_card_data(
            cds_ct_utils:lookup(token),
            cds_ct_utils:lookup(session),
            root_url(C)
        )
    ).

-spec get_card_data_backward_compatibility(config()) -> _.

get_card_data_backward_compatibility(C) ->
    CardData = #{
        pan => <<"4242424242424648">>,
        exp_date => #{month => 12,year => 3000},
        cardholder_name => <<"Tony Stark">>,
        cvv => <<>>
    },
    #{bank_card := #{token := Token}} = cds_old_cds_client:put_card(CardData, oldcds_url(C)),
    CDSCardClient = config(cds_storage_client, C),
    ?assertEqual(
        CDSCardClient:get_test_card(CardData, <<>>),
        CDSCardClient:get_card_data(Token, root_url(C))
    ).

-spec no_last_dot_in_token(config()) -> _.

no_last_dot_in_token(C) ->
    CardData = #{
        pan => <<"4242424242424648">>,
        exp_date => #{month => 12,year => 3000},
        cardholder_name => <<"Tony Stark">>,
        cvv => <<>>
    },
    CDSCardClient = config(cds_storage_client, C),
    #{bank_card := #{token := Token}} =
        CDSCardClient:put_card(
            CDSCardClient:get_test_card(CardData, <<>>),
            root_url(C)),
    LastByte = binary:part(Token, {byte_size(Token), -1}),
    ?assertNotEqual(<<".">>, LastByte).

-spec get_card_data_unavailable(config()) -> _.

get_card_data_unavailable(C) ->
    CDSCardClient = config(cds_storage_client, C),
    try CDSCardClient:get_card_data(<<"No matter what">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec get_session_card_data_unavailable(config()) -> _.

get_session_card_data_unavailable(C) ->
    CDSCardClient = config(cds_storage_client, C),
    try CDSCardClient:get_session_card_data(<<"TOKEN">>, <<"SESSION">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec put_card_data_unavailable(config()) -> _.

put_card_data_unavailable(C) ->
    CDSCardClient = config(cds_storage_client, C),
    try
        CDSCardClient:put_card_and_session(
            CDSCardClient:get_test_card(undefined),
            ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)
        )
    catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec put_card_data_3ds_unavailable(config()) -> _.

put_card_data_3ds_unavailable(C) ->
    CDSCardClient = config(cds_storage_client, C),
    try
        CDSCardClient:put_card_and_session(
            CDSCardClient:get_test_card(undefined),
            ?SESSION_DATA(?AUTH_3DS), root_url(C)
        )
    catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec put_card_data_no_member(config()) -> _.

put_card_data_no_member(C) ->
    CDSCardClient = config(cds_storage_client, C),
    try
        CDSCardClient:put_card_and_session(
            CDSCardClient:get_test_card(undefined),
            ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)
        )
    catch
        error:{woody_error, {external, resource_unavailable, <<"{pool_error,no_members}">>}} ->
            ok
    end.

-spec session_cleaning(config()) -> _.

session_cleaning(C) ->
    CDSCardClient = config(cds_storage_client, C),
    #{
        bank_card := #{
            token := Token
        },
        session_id := Session
    } = CDSCardClient:put_card_and_session(
        CDSCardClient:get_test_card(undefined),
        ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)
    ),

    ?assertEqual(
        CDSCardClient:get_test_card(<<>>),
        CDSCardClient:get_card_data(Token, root_url(C))
    ),
    ?SESSION_DATA_MATCH(?CARD_SEC_CODE_MATCH(?CVV)) = CDSCardClient:get_session_data(Session, root_url(C)),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),

    ok = timer:sleep(Lifetime * 1000 + Interval * 2),
    _ = ?assertEqual({error, session_data_not_found}, CDSCardClient:get_session_data(Session, root_url(C))),
    _ = ?assertEqual(
        CDSCardClient:get_test_card(<<>>),
        CDSCardClient:get_card_data(Token, root_url(C))).

-spec refresh_sessions(config()) -> _.

refresh_sessions(C) ->
    CDSCardClient = config(cds_storage_client, C),
    #{
        bank_card := #{
            token := Token
        },
        session_id := Session
    } = CDSCardClient:put_card_and_session(
        CDSCardClient:get_test_card(undefined),
        ?SESSION_DATA(?CARD_SEC_CODE(<<"345">>)), root_url(C)
    ),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),

    [
        begin
            _ = ?assertEqual(ok, catch cds_maintenance:refresh_sessions_created_at()),
            timer:sleep((Lifetime * 1000) div 4)
        end
        || _ <- lists:seq(1, 6)],

    ok = timer:sleep(Interval),
    _ = ?assertMatch(?SESSION_DATA_MATCH(_), CDSCardClient:get_session_data(Session, root_url(C))),
    ok = timer:sleep(Lifetime * 1000 + Interval),

    _ = ?assertEqual({error, session_data_not_found}, CDSCardClient:get_session_data(Session, root_url(C))),
    _ = ?assertEqual(
        CDSCardClient:get_test_card(<<>>),
        CDSCardClient:get_card_data(Token, root_url(C))
    ).

-spec recrypt(config()) -> _.

%% dishonest test which uses external functions
recrypt(C) ->
    {KeyID0, _} = cds_keyring:get_current_key(),
    CardholderData = #{
        cardnumber => <<"5321301234567892">>,
        exp_date => {12, 3000},
        cardholder => <<"Tony Stark">>
    },
    SessionDataCVV = #{auth_data => #{type => cvv, value => <<"345">>}},

    Token      = cds:put_card(cds_card_data:marshal_cardholder_data(CardholderData)),
    SessionCVV = genlib:unique(),
    ok = cds:put_session(SessionCVV, cds_card_data:marshal_session_data(SessionDataCVV)),

    SessionData3DS = #{auth_data => #{type => '3ds', cryptogram => <<"cryptogram">>, eci => <<"5">>}},

    Session3DS = genlib:unique(),
    ok = cds:put_session(Session3DS, cds_card_data:marshal_session_data(SessionData3DS)),

    {EncryptedCardDataCVV0, EncryptedSessionDataCVV0} = cds_card_storage:get_session_card_data(Token, SessionCVV),
    <<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>> = EncryptedCardDataCVV0,
    {<<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>>,
        <<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>>} = EncryptedSessionDataCVV0,

    {EncryptedCardData3DS0, EncryptedSessionData3DS0} = cds_card_storage:get_session_card_data(Token, Session3DS),
    <<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>> = EncryptedCardData3DS0,
    {<<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>>,
        <<?ENCRYPT_VERSION, KeyID0:8/integer-big-unsigned-unit:8, _/binary>>} = EncryptedSessionData3DS0,

    rotate(C),
    [{recrypting, #{
        interval := Interval
    }}] = config(recrypting_config, C),

    % we should meet reencryption at least once _after_ rotation
    {ok, KeyringFetchInterval} = application:get_env(cds, keyring_fetch_interval),
    _ = timer:sleep(Interval * 2 + KeyringFetchInterval * 2),
    {KeyID, _} = cds_keyring:get_current_key(),
    true = (KeyID0 =/= KeyID),
    {EncryptedCardDataCVV, EncryptedSessionDataCVV} = cds_card_storage:get_session_card_data(Token, SessionCVV),
    <<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8, _/binary>> = EncryptedCardDataCVV,
    {<<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8, _/binary>>,
        <<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8, _/binary>>} = EncryptedSessionDataCVV,

    {EncryptedCardData3DS, EncryptedSessionData3DS} = cds_card_storage:get_session_card_data(Token, Session3DS),
    <<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8, _/binary>> = EncryptedCardData3DS,
    {<<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8,
        _/binary>>, <<?ENCRYPT_VERSION, KeyID:8/integer-big-unsigned-unit:8, _/binary>>} = EncryptedSessionData3DS.

%%
%% helpers
%%


config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

update_config(Key, Config, NewVal) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, _Val}} ->
            lists:keyreplace(Key, 1, Config, {Key, NewVal});
        _ ->
            [{Key, NewVal} | Config]
    end.

root_url(C) ->
    config(root_url, C).

oldcds_url(C) ->
    config(oldcds_root_url, C).
