-module(cds_ct_keyring).

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

-spec init(config()) -> _.

init(C) ->
    EncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_init(DecryptedMasterKeyShares, C),
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
    cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C));
validate_init([{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares], C) ->
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    {more_keys_needed, DecryptedMasterKeySharesCount} =
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C)),
    validate_init(DecryptedMasterKeyShares, C).


-spec rekey(config()) -> _.

rekey(C) ->
    ok = cds_keyring_client:start_rekey(2, root_url(C)),
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rekey(Id1, MasterKey1, root_url(C)),
    ok = cds_keyring_client:confirm_rekey(Id2, MasterKey2, root_url(C)),
    EncryptedMasterKeyShares = cds_keyring_client:start_rekey_validation(root_url(C)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares = decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_rekey(DecryptedMasterKeyShares, C),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec validate_rekey([{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}], config()) -> ok.

validate_rekey([{Id, DecryptedMasterKeyShare} | []], C) ->
    cds_keyring_client:validate_rekey(Id, DecryptedMasterKeyShare, root_url(C));
validate_rekey([{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares], C) ->
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    {more_keys_needed, DecryptedMasterKeySharesCount} =
        cds_keyring_client:validate_rekey(Id, DecryptedMasterKeyShare, root_url(C)),
    validate_rekey(DecryptedMasterKeyShares, C).

-spec lock(config()) -> _.

lock(C) ->
    ok = cds_keyring_client:lock(root_url(C)).

-spec unlock(config()) -> _.

unlock(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    ok = cds_keyring_client:start_unlock(root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_unlock(Id1, MasterKey1, root_url(C)),
    ok = cds_keyring_client:confirm_unlock(Id2, MasterKey2, root_url(C)).

-spec rotate(config()) -> _.

rotate(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    ok = cds_keyring_client:start_rotate(root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:confirm_rotate(Id1, MasterKey1, root_url(C)),
    ok = cds_keyring_client:confirm_rotate(Id2, MasterKey2, root_url(C)).

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
