-module(cds_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("shamir/include/shamir.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).
-export([init_with_timeout/1]).
-export([init_with_cancel/1]).
-export([lock/1]).
-export([unlock/1]).
-export([unlock_with_timeout/1]).
-export([rekey/1]).
-export([rekey_with_timeout/1]).
-export([rekey_with_cancel/1]).
-export([put_card_data/1]).
-export([get_card_data/1]).
-export([get_session_data/1]).
-export([put_card/1]).
-export([put_session/1]).
-export([put_card_data_3ds/1]).
-export([get_card_data_3ds/1]).
-export([get_session_data_3ds/1]).
-export([put_card_data_backward_compatibilty/1]).
-export([get_card_data_backward_compatibilty/1]).
-export([get_session_data_backward_compatibilty/1]).
-export([get_session_card_data_backward_compatibilty/1]).
-export([rotate/1]).
-export([rotate_with_timeout/1]).
-export([rotate_with_cancel/1]).
-export([recrypt/1]).
-export([session_cleaning/1]).
-export([refresh_sessions/1]).
-export([init_invalid_status/1]).
-export([init_invalid_args/1]).
-export([init_operation_aborted_failed_to_recover/1]).
-export([init_operation_aborted_failed_to_decrypt/1]).
-export([init_operation_aborted_non_matching_mk/1]).
-export([lock_invalid_status/1]).
-export([rotate_invalid_status/1]).
-export([rekey_invalid_args/1]).
-export([rekey_invalid_status/1]).
-export([rekey_operation_aborted_failed_to_decrypt_keyring/1]).
-export([rekey_operation_aborted_failed_to_recover_confirm/1]).
-export([rekey_operation_aborted_failed_to_recover_validate/1]).
-export([rekey_operation_aborted_non_matching_masterkey/1]).
-export([rekey_operation_aborted_wrong_masterkey/1]).
-export([rotate_failed_to_recover/1]).
-export([rotate_wrong_masterkey/1]).
-export([put_card_data_unavailable/1]).
-export([put_card_data_3ds_unavailable/1]).
-export([get_card_data_unavailable/1]).
-export([get_session_card_data_unavailable/1]).
-export([put_card_data_no_member/1]).

-export([decrypt_and_sign_masterkeys/3]).
-export([validate_init/2]).

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

-spec test() -> _.

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
        {riak_storage_backend, [], [
            {group, general_flow},
            {group, error_map}
        ]},
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
            rekey,
            rekey_with_timeout,
            rekey_with_cancel,
            put_card_data,
            get_card_data,
            rotate_with_timeout,
            get_session_data,
            put_card,
            put_session,
            rotate,
            put_card_data_3ds,
            rotate_with_timeout,
            get_card_data_3ds,
            get_session_data_3ds,
            rotate,
            put_card_data_backward_compatibilty,
            rotate_with_timeout,
            get_card_data_backward_compatibilty,
            get_session_data_backward_compatibilty,
            get_session_card_data_backward_compatibilty,
            rotate,
            {group, hash_collision_check}
        ]},
        {keyring_errors, [sequence], [
            get_card_data_unavailable,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            lock_invalid_status,
            init_invalid_args,
            init_with_cancel,
            init_with_timeout,
            init_operation_aborted_failed_to_recover,
            init_operation_aborted_failed_to_decrypt,
            init_operation_aborted_non_matching_mk,
            init,
            init_invalid_status,
            lock,
            lock,
            unlock_with_timeout,
            lock,
            init_invalid_status,
            rotate_invalid_status,
            rekey_invalid_status,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            get_card_data_unavailable,
            get_session_card_data_unavailable,
            unlock,
            rekey_invalid_args,
            rekey_operation_aborted_failed_to_decrypt_keyring,
            rekey_operation_aborted_failed_to_recover_confirm,
            rekey_operation_aborted_failed_to_recover_validate,
            rekey_operation_aborted_non_matching_masterkey,
            rekey_operation_aborted_wrong_masterkey,
            rotate_with_cancel,
            rotate_failed_to_recover,
            rotate_wrong_masterkey
        ]},
        {session_management, [sequence], [
            init,
            lock,
            unlock,
            session_cleaning,
            refresh_sessions,
            recrypt
        ]},
        {hash_collision_check, [parallel], [
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
        ]}
    ].
%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(riak_storage_backend, C) ->
    cds_ct_utils:set_riak_storage(C);

init_per_group(ets_storage_backend, C) ->
    cds_ct_utils:set_ets_storage(C);

init_per_group(general_flow, C) ->
    C;
init_per_group(hash_collision_check, C) ->
    Stash = config(stash, C),
    C1 = cds_ct_utils:start_clear(C, Stash),
    C1 ++ C;

init_per_group(keyring_errors, C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    C1 = cds_ct_utils:start_clear([{storage_config, StorageConfig} | C]),
    C1 ++ C;

init_per_group(session_management, C) ->
    CleanerConfig = [
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
            interval => 1000
        }}
    ],
    C1 = [{recrypting_config, Recrypting}, {session_cleaning_config, CleanerConfig} | C],
    C2 = cds_ct_utils:start_clear(C1),
    C1 ++ C2;

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
    cds_ct_utils:start_clear([{storage_config, StorageConfigNew} | C]);
init_per_group(error_map_ddos, C) ->
    C;

init_per_group(_, C) ->
    C1 = cds_ct_utils:start_clear(C),
    C1 ++ C.

-spec end_per_group(atom(), config()) -> _.

end_per_group(Group, C) when
    Group =:= ets_storage_backend;
    Group =:= general_flow;
    Group =:= riak_storage_backend;
    Group =:= error_map_ddos
 ->
    C;

end_per_group(_, C) ->
    cds_ct_utils:stop_clear(C).

%%
%% tests
%%

-spec init(config()) -> _.

init(C) ->
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    _ = ?assertMatch(
        #'KeyringState'{
            status = not_initialized,
            activities = #'ActivitiesState'{
                initialization = #'InitializationState'{
                    phase = validation,
                    validation_shares = #{}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    ok = validate_init(DecryptedMasterKeyShares, C),
    _ = ?assertMatch(
        #'KeyringState'{
            status = unlocked,
            activities = #'ActivitiesState'{
                initialization = #'InitializationState'{
                    phase = uninitialized,
                    validation_shares = #{}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec init_with_timeout(config()) -> _.

init_with_timeout(C) ->
    {Id, DecryptedMasterKeyShare} = partial_init(C),
    Timeout = genlib_app:env(cds, keyring_rotation_lifetime, 4000),
    ok = timer:sleep(Timeout + 1500),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {initialization, uninitialized}},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ).

-spec init_with_cancel(config()) -> _.

init_with_cancel(C) ->
    {Id, DecryptedMasterKeyShare} = partial_init(C),
    ok = cds_keyring_client:cancel_init(root_url(C)),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {initialization, uninitialized}},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ).

partial_init(C) ->
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {initialization, validation}},
        cds_keyring_client:start_init(2, root_url(C))
    ),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    [{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares] =
        decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    _ = ?assertEqual(
        {more_keys_needed, DecryptedMasterKeySharesCount},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    _ = ?assertEqual(
        {more_keys_needed, DecryptedMasterKeySharesCount},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    {Id, DecryptedMasterKeyShare}.

-spec decrypt_and_sign_masterkeys(cds_keysharing:encrypted_master_key_shares(), map(), map()) ->
    [{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}].

decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys) ->
    lists:map(
        fun
            (#{id := Id, owner := Owner, encrypted_share := EncryptedShare}) ->
                {ok, #{id := Id, owner := Owner}} = cds_shareholder:get_by_id(Id),
                EncPrivateKey = maps:get(Id, EncPrivateKeys),
                SigPrivateKey = maps:get(Id, SigPrivateKeys),
                DecryptedShare = cds_crypto:private_decrypt(EncPrivateKey, EncryptedShare),
                {Id, cds_crypto:sign(SigPrivateKey, DecryptedShare)}
        end,
        EncryptedMasterKeyShares).

-spec validate_init([{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}], config()) -> ok.

validate_init([{Id, DecryptedMasterKeyShare} | []], C) ->
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    ok;
validate_init([{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares], C) ->
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    _ = ?assertEqual(
        {more_keys_needed, DecryptedMasterKeySharesCount},
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    validate_init(DecryptedMasterKeyShares, C).

-spec lock(config()) -> _.

lock(C) ->
    ok = cds_keyring_client:lock(root_url(C)).

-spec unlock(config()) -> _.

unlock(C) ->
    _ = ?assertMatch(
        #'KeyringState'{
            status = locked,
            activities = #'ActivitiesState'{
                unlock = #'UnlockState'{
                    phase = uninitialized
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_unlock(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C))),
    _ = ?assertMatch(
        #'KeyringState'{
            status = locked,
            activities = #'ActivitiesState'{
                unlock = #'UnlockState'{
                    phase = validation,
                    confirmation_shares = #{1 := Id1}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    _ = ?assertEqual({success, #'Success'{}}, cds_keyring_client:confirm_unlock(Id2, MasterKey2, root_url(C))).

-spec unlock_with_timeout(config()) -> _.

unlock_with_timeout(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_unlock(root_url(C))),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {unlock, validation}},
        cds_keyring_client:start_unlock(root_url(C))
    ),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C))),
    Timeout = genlib_app:env(cds, keyring_unlock_lifetime, 1000),
    timer:sleep(Timeout + 500),
    _ = ?assertEqual(ok, cds_keyring_client:start_unlock(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C))),
    _ = ?assertEqual({success, #'Success'{}}, cds_keyring_client:confirm_unlock(Id2, MasterKey2, root_url(C))).

-spec rekey(config()) -> _.

rekey(C) ->
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {rekeying, confirmation}},
        cds_keyring_client:start_rekey(2, root_url(C))
    ),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {rekeying, postconfirmation}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_rekey_validation(root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    _ = ?assertMatch(
        #'KeyringState'{
            status = unlocked,
            activities = #'ActivitiesState'{
                rekeying = #'RekeyingState'{
                    phase = validation,
                    confirmation_shares = #{1 := Id1, 2 := Id2},
                    validation_shares = #{}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_rekey(DecryptedMasterKeyShares, C),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec rekey_with_timeout(config()) -> _.

rekey_with_timeout(C) ->
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    Timeout = genlib_app:env(cds, keyring_rekeying_lifetime, 1000),
    timer:sleep(Timeout + 500),
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_rekey_validation(root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_rekey(DecryptedMasterKeyShares, C),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec rekey_with_cancel(config()) -> _.

rekey_with_cancel(C) ->
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    _ = ?assertEqual(ok, cds_keyring_client:cancel_rekey(root_url(C))),
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_rekey_validation(root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_rekey(DecryptedMasterKeyShares, C),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec validate_rekey([{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}], config()) -> ok.

validate_rekey([{Id, DecryptedMasterKeyShare} | []], C) ->
    _ = ?assertEqual(
        {success, #'Success'{}},
        cds_keyring_client:validate_rekey(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    ok;
validate_rekey([{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares], C) ->
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    _ = ?assertEqual(
        {more_keys_needed, DecryptedMasterKeySharesCount},
        cds_keyring_client:validate_rekey(Id, DecryptedMasterKeyShare, root_url(C))
    ),
    validate_rekey(DecryptedMasterKeyShares, C).

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
    #'PutCardDataResult'{} = cds_card_client:put_card_data(
        CardData,
        ?SESSION_DATA(?CARD_SEC_CODE(<<"123">>)),
        root_url(C)
    ),
    % check with cardholder
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)),
    cds_ct_utils:store([{token, Token}, {session, Session}], C).

-spec get_card_data(config()) -> _.

get_card_data(C) ->
    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(
        cds_ct_utils:lookup(token, C),
        root_url(C)
    ).

-spec get_session_data(config()) -> _.

get_session_data(C) ->
    ?SESSION_DATA(?CARD_SEC_CODE(?CVV)) = cds_card_client:get_session_data(
        cds_ct_utils:lookup(session, C),
        root_url(C)
    ).

-spec put_card(config()) -> _.

put_card(C) ->
    CardData = #'CardData'{
        pan = <<"4242424242424648">>,
        exp_date = #'ExpDate'{
            month = 11,
            year = 3000
        }
    },
    #'PutCardResult'{
        bank_card = #domain_BankCard{
            token = Token
        }
    } = cds_card_client:put_card(CardData, root_url(C)),
    CardData2 = CardData#'CardData'{cvv = <<>>},
    CardData2 = cds_card_client:get_card_data(Token, root_url(C)).

-spec put_session(config()) -> _.

put_session(C) ->
    SessionID = crypto:strong_rand_bytes(16),
    SessionData = ?SESSION_DATA(?CARD_SEC_CODE(?CVV)),
    ok = cds_card_client:put_session(SessionID, SessionData, root_url(C)),
    SessionData = cds_card_client:get_session_data(SessionID, root_url(C)).

-spec put_card_data_3ds(config()) -> _.

put_card_data_3ds(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_card_client:put_card_data(
        ?CREDIT_CARD(undefined),
        ?SESSION_DATA(?AUTH_3DS),
        root_url(C)
    ),
    cds_ct_utils:store([{token, Token}, {session, Session}], C).

-spec get_card_data_3ds(config()) -> _.

get_card_data_3ds(C) ->
    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(cds_ct_utils:lookup(token, C), root_url(C)).

-spec get_session_data_3ds(config()) -> _.

get_session_data_3ds(C) ->
    ?SESSION_DATA(?AUTH_3DS) = cds_card_client:get_session_data(cds_ct_utils:lookup(session, C), root_url(C)).

-spec put_card_data_backward_compatibilty(config()) -> _.

put_card_data_backward_compatibilty(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_card_client:put_card_data(?CREDIT_CARD(?CVV), root_url(C)),
    cds_ct_utils:store([{token, Token}, {session, Session}], C).

-spec get_card_data_backward_compatibilty(config()) -> _.

get_card_data_backward_compatibilty(C) ->
    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(cds_ct_utils:lookup(token, C), root_url(C)).

-spec get_session_data_backward_compatibilty(config()) -> _.

get_session_data_backward_compatibilty(C) ->
    ?SESSION_DATA(?CARD_SEC_CODE(?CVV)) = cds_card_client:get_session_data(
        cds_ct_utils:lookup(session, C),
        root_url(C)
    ).

-spec get_session_card_data_backward_compatibilty(config()) -> _.

get_session_card_data_backward_compatibilty(C) ->
    ?CREDIT_CARD(?CVV) = cds_card_client:get_session_card_data(
        cds_ct_utils:lookup(token, C),
        cds_ct_utils:lookup(session, C),
        root_url(C)
    ).

-spec rotate(config()) -> _.

rotate(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))),
    _ = ?assertEqual({success, #'Success'{}}, cds_keyring_client:confirm_rotate(Id2, MasterKey2, root_url(C))).

-spec rotate_with_timeout(config()) -> _.

rotate_with_timeout(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, {Id3, MasterKey3}] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))),
    Timeout = genlib_app:env(cds, keyring_rotation_lifetime, 1000),
    timer:sleep(Timeout + 500),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id2, MasterKey2, root_url(C))),
    _ = ?assertEqual({success, #'Success'{}}, cds_keyring_client:confirm_rotate(Id3, MasterKey3, root_url(C))).

-spec rotate_with_cancel(config()) -> _.

rotate_with_cancel(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, {Id3, MasterKey3}] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))),
    _ = ?assertEqual(ok, cds_keyring_client:cancel_rotate(root_url(C))),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id2, MasterKey2, root_url(C))),
    _ = ?assertEqual({success, #'Success'{}}, cds_keyring_client:confirm_rotate(Id3, MasterKey3, root_url(C))).

-spec init_invalid_status(config()) -> _.

init_invalid_status(C) ->
    _ = ?assertThrow(
        #'InvalidStatus'{status = _SomeStatus},
        cds_keyring_client:start_init(2, root_url(C))
    ).

-spec init_invalid_args(config()) -> _.

init_invalid_args(C) ->
    _ = ?assertThrow(
        #'InvalidArguments'{},
        cds_keyring_client:start_init(4, root_url(C))
    ),
    _ = ?assertThrow(
        #'InvalidArguments'{},
        cds_keyring_client:start_init(0, root_url(C))
    ).

-spec init_operation_aborted_failed_to_recover(config()) -> _.

init_operation_aborted_failed_to_recover(C) ->
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),
    InvalidShare = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),

    _ = cds_keyring_client:start_init(2, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_recover">>},
        cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, InvalidShare), root_url(C))
    ).

-spec init_operation_aborted_failed_to_decrypt(config()) -> _.

init_operation_aborted_failed_to_decrypt(C) ->
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),

    _ = cds_keyring_client:start_init(2, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_decrypt_keyring">>},
        cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, WrongShare3), root_url(C))
    ).

-spec init_operation_aborted_non_matching_mk(config()) -> _.

init_operation_aborted_non_matching_mk(C) ->
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 1, 3),
    MasterKey2 = cds_crypto:key(),
    [_WrongShare4, _WrongShare5, WrongShare6] = cds_keysharing:share(MasterKey2, 1, 3),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),

    _ = cds_keyring_client:start_init(1, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"non_matching_masterkey">>},
        cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, WrongShare6), root_url(C))
    ).

-spec rekey_operation_aborted_wrong_masterkey(config()) -> _.

rekey_operation_aborted_wrong_masterkey(C) ->
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {_Id3, _SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),

    ok = cds_keyring_client:start_rekey(2, root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"wrong_masterkey">>},
        cds_keyring_client:confirm_rekey(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C))
    ).

-spec rekey_operation_aborted_failed_to_recover_confirm(config()) -> _.

rekey_operation_aborted_failed_to_recover_confirm(C) ->
    MasterKey = cds_crypto:key(),
    [WrongShare1, _WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),
    InvalidShare = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {_Id3, _SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),

    ok = cds_keyring_client:start_rekey(2, root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_recover">>},
        cds_keyring_client:confirm_rekey(Id2, cds_crypto:sign(SigPrivateKey2, InvalidShare), root_url(C))
    ).

-spec rekey_operation_aborted_failed_to_recover_validate(config()) -> _.

rekey_operation_aborted_failed_to_recover_validate(C) ->
    [{Id1, TrueSignedShare1}, {Id2, TrueSignedShare2} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),
    InvalidShare = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),

    ok = cds_keyring_client:start_rekey(2, root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, TrueSignedShare1, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:confirm_rekey(Id2, TrueSignedShare2, root_url(C)),
    _ = cds_keyring_client:start_rekey_validation(root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_rekey(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_rekey(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_recover">>},
        cds_keyring_client:validate_rekey(Id3, cds_crypto:sign(SigPrivateKey3, InvalidShare), root_url(C))
    ).

-spec rekey_operation_aborted_failed_to_decrypt_keyring(config()) -> _.

rekey_operation_aborted_failed_to_decrypt_keyring(C) ->
    [{Id1, TrueSignedShare1}, {Id2, TrueSignedShare2} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, WrongShare3] = cds_keysharing:share(MasterKey, 2, 3),

    ok = cds_keyring_client:start_rekey(2, root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, TrueSignedShare1, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:confirm_rekey(Id2, TrueSignedShare2, root_url(C)),
    _ = cds_keyring_client:start_rekey_validation(root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_rekey(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_decrypt_keyring">>},
        cds_keyring_client:validate_rekey(Id3, cds_crypto:sign(SigPrivateKey3, WrongShare3), root_url(C))
    ).


-spec rekey_operation_aborted_non_matching_masterkey(config()) -> _.

rekey_operation_aborted_non_matching_masterkey(C) ->
    [{Id1, TrueSignedShare1}, {Id2, TrueSignedShare2} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),
    MasterKey = cds_crypto:key(),
    [WrongShare1, WrongShare2, _WrongShare3] = cds_keysharing:share(MasterKey, 1, 3),
    MasterKey2 = cds_crypto:key(),
    [_WrongShare4, _WrongShare5, WrongShare6] = cds_keysharing:share(MasterKey2, 1, 3),

    ok = cds_keyring_client:start_rekey(1, root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, TrueSignedShare1, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:confirm_rekey(Id2, TrueSignedShare2, root_url(C)),
    _ = cds_keyring_client:start_rekey_validation(root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_rekey(Id1, cds_crypto:sign(SigPrivateKey1, WrongShare1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_rekey(Id2, cds_crypto:sign(SigPrivateKey2, WrongShare2), root_url(C)),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"non_matching_masterkey">>},
        cds_keyring_client:validate_rekey(Id3, cds_crypto:sign(SigPrivateKey3, WrongShare6), root_url(C))
    ).

-spec rekey_invalid_args(config()) -> _.

rekey_invalid_args(C) ->
    _ = ?assertThrow(
        #'InvalidArguments'{},
        cds_keyring_client:start_rekey(4, root_url(C))
    ),
    _ = ?assertThrow(
        #'InvalidArguments'{},
        cds_keyring_client:start_rekey(0, root_url(C))
    ).

-spec rekey_invalid_status(config()) -> _.

rekey_invalid_status(C) ->
    _ = ?assertThrow(
        #'InvalidStatus'{status = _SomeStatus},
        cds_keyring_client:start_rekey(2, root_url(C))
    ).

-spec lock_invalid_status(config()) -> _.

lock_invalid_status(C) ->
    _ = ?assertThrow(
        #'InvalidStatus'{status = _SomeStatus},
        cds_keyring_client:lock(root_url(C))
    ).

-spec rotate_invalid_status(config()) -> _.

rotate_invalid_status(C) ->
    _ = ?assertThrow(
        #'InvalidStatus'{status = _SomeStatus},
        cds_keyring_client:start_rotate(root_url(C))
    ).

-spec rotate_failed_to_recover(config()) -> _.

rotate_failed_to_recover(C) ->
    [{Id1, MasterKey1}, {Id2, _MasterKey2} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    MasterKey2 = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),
    SigPrivateKeys = sig_private_keys(C),
    [_SigPrivateKey1, SigPrivateKey2, _SigPrivateKey3] = maps:values(SigPrivateKeys),
    _ = ?assertEqual(
        ok,
        cds_keyring_client:start_rotate(root_url(C))
    ),
    _ = ?assertThrow(
        #'InvalidActivity'{activity = {rotation, validation}},
        cds_keyring_client:start_rotate(root_url(C))
    ),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))
    ),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"failed_to_recover">>},
        cds_keyring_client:confirm_rotate(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C))
    ).

-spec rotate_wrong_masterkey(config()) -> _.

rotate_wrong_masterkey(C) ->
    MasterKey = cds_crypto:key(),
    [MasterKey1, MasterKey2, _MasterKey3] = cds_keysharing:share(MasterKey, 2, 3),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, _SigPrivateKey3] = maps:to_list(SigPrivateKeys),
    _ = ?assertEqual(
        ok,
        cds_keyring_client:start_rotate(root_url(C))
    ),
    _ = ?assertEqual(
        {more_keys_needed, 1},
        cds_keyring_client:confirm_rotate(Id1, cds_crypto:sign(SigPrivateKey1, MasterKey1), root_url(C))
    ),
    _ = ?assertThrow(
        #'OperationAborted'{reason = <<"wrong_masterkey">>},
        cds_keyring_client:confirm_rotate(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C))
    ).

-spec get_card_data_unavailable(config()) -> _.

get_card_data_unavailable(C) ->
    try cds_card_client:get_card_data(<<"No matter what">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec get_session_card_data_unavailable(config()) -> _.

get_session_card_data_unavailable(C) ->
    try cds_card_client:get_session_card_data(<<"TOKEN">>, <<"SESSION">>, root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} -> ok
    end.

-spec put_card_data_unavailable(config()) -> _.

put_card_data_unavailable(C) ->
    try cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec put_card_data_3ds_unavailable(config()) -> _.

put_card_data_3ds_unavailable(C) ->
    try cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?AUTH_3DS), root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, _}} ->
            ok
    end.

-spec put_card_data_no_member(config()) -> _.

put_card_data_no_member(C) ->
    try cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)) catch
        error:{woody_error, {external, resource_unavailable, <<"{pool_error,no_members}">>}} ->
            ok
    end.

-spec session_cleaning(config()) -> _.

session_cleaning(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(?CVV)), root_url(C)),

    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(Token, root_url(C)),
    ?CREDIT_CARD(?CVV) = cds_card_client:get_session_card_data(Token, Session, root_url(C)),

    [{session_cleaning, #{
        session_lifetime := Lifetime,
        interval := Interval
    }}] = config(session_cleaning_config, C),

    ok = timer:sleep(Lifetime * 1000 + Interval * 2),
    _ = ?assertThrow( #'CardDataNotFound'{}, cds_card_client:get_session_card_data(Token, Session, root_url(C))),
    _ = ?assertMatch(?CREDIT_CARD(<<>>), cds_card_client:get_card_data(Token, root_url(C))).

-spec refresh_sessions(config()) -> _.

refresh_sessions(C) ->
    #'PutCardDataResult'{
        bank_card = #domain_BankCard{
            token = Token
        },
        session_id = Session
    } = cds_card_client:put_card_data(?CREDIT_CARD(undefined), ?SESSION_DATA(?CARD_SEC_CODE(<<"345">>)), root_url(C)),

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
    _ = ?assertMatch(?CREDIT_CARD(_), cds_card_client:get_session_card_data(Token, Session, root_url(C))),
    ok = timer:sleep(Lifetime * 1000 + Interval),

    _ = ?assertThrow( #'CardDataNotFound'{}, cds_card_client:get_session_card_data(Token, Session, root_url(C))),
    _ = ?assertMatch(?CREDIT_CARD(<<>>), cds_card_client:get_card_data(Token, root_url(C))).


-spec recrypt(config()) -> _.

%% dishonest test which uses external functions
recrypt(C) ->
    {KeyID0, _} = cds_keyring_manager:get_current_key(),
    CardholderData = #{
        cardnumber => <<"5321301234567892">>,
        exp_date => {12, 3000},
        cardholder => <<"Tony Stark">>
    },
    SessionDataCVV = #{auth_data => #{type => cvv, value => <<"345">>}},
    {TokenCVV, SessionCVV} = cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionDataCVV)
    }),

    SessionData3DS = #{auth_data => #{type => '3ds', cryptogram => <<"cryptogram">>, eci => <<"5">>}},
    {Token3DS, Session3DS} = cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionData3DS)
    }),

    {EncryptedCardDataCVV0, EncryptedSessionDataCVV0} = cds_card_storage:get_session_card_data(TokenCVV, SessionCVV),
    <<KeyID0, _/binary>> = EncryptedCardDataCVV0,
    {<<KeyID0, _/binary>>, <<KeyID0, _/binary>>} = EncryptedSessionDataCVV0,

    {EncryptedCardData3DS0, EncryptedSessionData3DS0} = cds_card_storage:get_session_card_data(Token3DS, Session3DS),
    <<KeyID0, _/binary>> = EncryptedCardData3DS0,
    {<<KeyID0, _/binary>>, <<KeyID0, _/binary>>} = EncryptedSessionData3DS0,


    rotate(C),
    [{recrypting, #{
        interval := Interval
    }}] = config(recrypting_config, C),

    % we should meet reencryption at least once _after_ rotation
    _ = timer:sleep(Interval * 2),
    {KeyID, _} = cds_keyring_manager:get_current_key(),
    true = (KeyID0 =/= KeyID),
    {EncryptedCardDataCVV, EncryptedSessionDataCVV} = cds_card_storage:get_session_card_data(TokenCVV, SessionCVV),
    <<KeyID, _/binary>> = EncryptedCardDataCVV,
    {<<KeyID, _/binary>>, <<KeyID, _/binary>>} = EncryptedSessionDataCVV,

    {EncryptedCardData3DS, EncryptedSessionData3DS} = cds_card_storage:get_session_card_data(Token3DS, Session3DS),
    <<KeyID, _/binary>> = EncryptedCardData3DS,
    {<<KeyID, _/binary>>, <<KeyID, _/binary>>} = EncryptedSessionData3DS.

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

enc_private_keys(C) ->
    config(enc_private_keys, C).

sig_private_keys(C) ->
    config(sig_private_keys, C).
