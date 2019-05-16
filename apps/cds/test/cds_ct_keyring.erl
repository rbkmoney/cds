-module(cds_ct_keyring).

-include_lib("eunit/include/eunit.hrl").

-export([init/1]).
-export([rekey/1]).
-export([lock/1]).
-export([unlock/1]).
-export([rotate/1]).

-export([decrypt_and_sign_masterkeys/3]).
-export([validate_init/2]).
-export([validate_rekey/2]).

%%
%% Internal types
%%

-type config() :: [{atom(), any()}] | atom().

-spec test() -> _.

-spec init(config()) -> _.

init(C) ->
    EncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    _ = ?assertMatch(
        #{
            status := not_initialized,
            activities := #{
                initialization := #{
                    phase := validation,
                    validation_shares := #{}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    ok = validate_init(DecryptedMasterKeyShares, C),
    _ = ?assertMatch(
        #{
            status := unlocked,
            activities := #{
                initialization := #{
                    phase := uninitialized,
                    validation_shares := #{}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

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
        ok,
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


-spec rekey(config()) -> _.

rekey(C) ->
    _ = ?assertEqual(ok, cds_keyring_client:start_rekey(2, root_url(C))),
    _ = ?assertEqual(
        {error, {invalid_activity, {rekeying, confirmation}}},
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
        ok,
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    _ = ?assertEqual(
        {error, {invalid_activity, {rekeying, postconfirmation}}},
        cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C))
    ),
    EncryptedMasterKeyShares = cds_keyring_client:start_rekey_validation(root_url(C)),
    _ = ?assertMatch(
        #{
            status := unlocked,
            activities := #{
                rekeying := #{
                    phase := validation,
                    confirmation_shares := #{1 := Id1, 2 := Id2},
                    validation_shares := #{}
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

-spec validate_rekey([{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}], config()) -> ok.

validate_rekey([{Id, DecryptedMasterKeyShare} | []], C) ->
    _ = ?assertEqual(
        ok,
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

-spec lock(config()) -> _.

lock(C) ->
    ok = cds_keyring_client:lock(root_url(C)).

-spec unlock(config()) -> _.

unlock(C) ->
    _ = ?assertMatch(
        #{
            status := locked,
            activities := #{
                unlock := #{
                    phase := uninitialized
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_unlock(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C))),
    _ = ?assertMatch(
        #{
            status := locked,
            activities := #{
                unlock := #{
                    phase := validation,
                    confirmation_shares := #{1 := Id1}
                }
            }
        },
        cds_keyring_client:get_state(root_url(C))
    ),
    _ = ?assertEqual(ok, cds_keyring_client:confirm_unlock(Id2, MasterKey2, root_url(C))).

-spec rotate(config()) -> _.

rotate(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    _ = ?assertEqual(ok, cds_keyring_client:start_rotate(root_url(C))),
    _ = ?assertEqual({more_keys_needed, 1}, cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C))),
    _ = ?assertEqual(ok, cds_keyring_client:confirm_rotate(Id2, MasterKey2, root_url(C))).

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

enc_private_keys(C) ->
    config(enc_private_keys, C).

sig_private_keys(C) ->
    config(sig_private_keys, C).
