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
-export([put_card_data/1]).
-export([get_card_data/1]).
-export([get_session_data/1]).
-export([put_card_data_3ds/1]).
-export([get_card_data_3ds/1]).
-export([get_session_data_3ds/1]).
-export([put_card_data_backward_compatibilty/1]).
-export([get_card_data_backward_compatibilty/1]).
-export([get_session_data_backward_compatibilty/1]).
-export([get_session_card_data_backward_compatibilty/1]).
-export([rotate/1]).
-export([rotate_with_timeout/1]).
-export([recrypt/1]).
-export([session_cleaning/1]).
-export([refresh_sessions/1]).
-export([init_invalid_status/1]).
-export([init_invalid_args/1]).
-export([init_operation_aborted/1]).
-export([lock_invalid_status/1]).
-export([rotate_keyring_locked/1]).
-export([rotate_failed_to_recover/1]).
-export([rotate_wrong_masterkey/1]).
-export([put_card_data_unavailable/1]).
-export([put_card_data_3ds_unavailable/1]).
-export([get_card_data_unavailable/1]).
-export([get_session_card_data_unavailable/1]).
-export([put_card_data_no_member/1]).

-export([decrypt_masterkeys/3]).
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
            rotate_with_timeout,
            get_session_data,
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
            init_operation_aborted,
            init,
            init_invalid_status,
            lock,
            lock,
            init_invalid_status,
            rotate_keyring_locked,
            put_card_data_unavailable,
            put_card_data_3ds_unavailable,
            get_card_data_unavailable,
            get_session_card_data_unavailable,
            unlock,
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
                session_lifetime => 7,
                batch_size => 1000,
                interval => 6000
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
    DecryptedMasterKeyShares = decrypt_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = validate_init(DecryptedMasterKeyShares, C),
    cds_ct_utils:store(master_keys, DecryptedMasterKeyShares, C).

-spec init_with_timeout(config()) -> _.

init_with_timeout(C) ->
    {Id, DecryptedMasterKeyShare} = partial_init(C),
    Timeout = genlib_app:env(cds, keyring_rotation_lifetime, 4000),
    ok = timer:sleep(Timeout + 1500),
    _ = ?assertEqual(
        #'InvalidActivity'{activity = {initialization, uninitialized}},
        (catch cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C)))).

-spec init_with_cancel(config()) -> _.

init_with_cancel(C) ->
    {Id, DecryptedMasterKeyShare} = partial_init(C),
    ok = cds_keyring_client:cancel_init(root_url(C)),
    #'InvalidActivity'{activity = {initialization, uninitialized}}
        = (catch cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C))).

partial_init(C) ->
    EncodedEncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(EncodedEncryptedMasterKeyShares),
    Shareholders = cds_shareholder:get_all(),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(Shareholders)),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    [{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares] =
        decrypt_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    _ = ?assertEqual({more_keys_needed, length(DecryptedMasterKeyShares)},
        (catch cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C)))),
    {Id, DecryptedMasterKeyShare}.

-spec decrypt_masterkeys(cds_keysharing:encrypted_master_key_shares(), map(), map()) ->
    [{cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()}].

decrypt_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys) ->
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
    {success, #'Success'{}} = cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C)),
    ok;
validate_init([{Id, DecryptedMasterKeyShare} | DecryptedMasterKeyShares], C) ->
    DecryptedMasterKeySharesCount = length(DecryptedMasterKeyShares),
    {more_keys_needed, DecryptedMasterKeySharesCount} =
        cds_keyring_client:validate_init(Id, DecryptedMasterKeyShare, root_url(C)),
    validate_init(DecryptedMasterKeyShares, C).

-spec lock(config()) -> _.

lock(C) ->
    ok = cds_keyring_client:lock(root_url(C)).

-spec unlock(config()) -> _.

unlock(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, _MasterKey3] = cds_ct_utils:lookup(master_keys, C),
    {more_keys_needed, 1} = cds_keyring_client:unlock(Id1, MasterKey1, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:unlock(Id2, MasterKey2, root_url(C)).

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
    {more_keys_needed, 1} = cds_keyring_client:rotate(Id1, MasterKey1, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:rotate(Id2, MasterKey2, root_url(C)).

-spec rotate_with_timeout(config()) -> _.

rotate_with_timeout(C) ->
    [{Id1, MasterKey1}, {Id2, MasterKey2}, {Id3, MasterKey3}] = cds_ct_utils:lookup(master_keys, C),
    {more_keys_needed, 1} = cds_keyring_client:rotate(Id1, MasterKey1, root_url(C)),
    Timeout = genlib_app:env(cds, keyring_rotation_lifetime, 1000),
    timer:sleep(Timeout + 500),
    {more_keys_needed, 1} = cds_keyring_client:rotate(Id2, MasterKey2, root_url(C)),
    {success, #'Success'{}} = cds_keyring_client:rotate(Id3, MasterKey3, root_url(C)).

-spec init_invalid_status(config()) -> _.

init_invalid_status(C) ->
    #'InvalidStatus'{status = _SomeStatus} = (catch cds_keyring_client:start_init(2, root_url(C))).

-spec init_invalid_args(config()) -> _.

init_invalid_args(C) ->
    #'InvalidArguments'{} = (catch cds_keyring_client:start_init(4, root_url(C))),
    #'InvalidArguments'{} = (catch cds_keyring_client:start_init(0, root_url(C))).

-spec init_operation_aborted(config) -> _.

init_operation_aborted(C) ->
    MK = cds_crypto:key(),
    [MasterKey1, MasterKey2, MasterKey3] = cds_keysharing:share(MK, 2, 3),
    MasterKey4 = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, {Id3, SigPrivateKey3}] =
        maps:to_list(SigPrivateKeys),
    MK2 = cds_crypto:key(),
    [MasterKey5, MasterKey6 | _] = cds_keysharing:share(MK2, 1, 3),
    MK3 = cds_crypto:key(),
    [_MasterKey7, _MasterKey8, MasterKey9] = cds_keysharing:share(MK3, 1, 3),

    _ = cds_keyring_client:start_init(2, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, MasterKey1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C)),
    Reason1 = atom_to_binary(failed_to_recover, utf8),
    #'OperationAborted'{reason = Reason1} = (catch cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, MasterKey4), root_url(C))),

    _ = cds_keyring_client:start_init(2, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, MasterKey1), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C)),
    Reason2 = atom_to_binary(failed_to_decrypt_keyring, utf8),
    #'OperationAborted'{reason = Reason2} = (catch cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, MasterKey3), root_url(C))),

    _ = cds_keyring_client:start_init(1, root_url(C)),
    {more_keys_needed, 2} = cds_keyring_client:validate_init(Id1, cds_crypto:sign(SigPrivateKey1, MasterKey5), root_url(C)),
    {more_keys_needed, 1} = cds_keyring_client:validate_init(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey6), root_url(C)),
    Reason3 = atom_to_binary(non_matching_masterkey, utf8),
    #'OperationAborted'{reason = Reason3} = (catch cds_keyring_client:validate_init(Id3, cds_crypto:sign(SigPrivateKey3, MasterKey9), root_url(C))).

-spec lock_invalid_status(config()) -> _.

lock_invalid_status(C) ->
    #'InvalidStatus'{status = _SomeStatus} = (catch cds_keyring_client:lock(root_url(C))).

-spec rotate_keyring_locked(config()) -> _.

rotate_keyring_locked(C) ->
    [{Id, MasterKey} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    #'InvalidStatus'{status = _SomeStatus} = (catch cds_keyring_client:rotate(Id, MasterKey, root_url(C))).

-spec rotate_failed_to_recover(config()) -> _.

rotate_failed_to_recover(C) ->
    [{Id1, MasterKey1}, {Id2, _MasterKey2} | _MasterKeys] = cds_ct_utils:lookup(master_keys, C),
    MasterKey2 = cds_keysharing:convert(#share{threshold = 2, x = 4, y = <<23224>>}),
    SigPrivateKeys = sig_private_keys(C),
    [_SigPrivateKey1, SigPrivateKey2, _SigPrivateKey3] = maps:values(SigPrivateKeys),
    {more_keys_needed, 1} = cds_keyring_client:rotate(Id1, MasterKey1, root_url(C)),
    Reason = atom_to_binary(failed_to_recover, utf8),
    #'OperationAborted'{reason = Reason} = (catch cds_keyring_client:rotate(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C))).

-spec rotate_wrong_masterkey(config()) -> _.

rotate_wrong_masterkey(C) ->
    MasterKey = cds_crypto:key(),
    [MasterKey1, MasterKey2, _MasterKey3] = cds_keysharing:share(MasterKey, 2, 3),
    SigPrivateKeys = sig_private_keys(C),
    [{Id1, SigPrivateKey1}, {Id2, SigPrivateKey2}, _SigPrivateKey3] = maps:to_list(SigPrivateKeys),
    {more_keys_needed, 1} = cds_keyring_client:rotate(Id1, cds_crypto:sign(SigPrivateKey1, MasterKey1), root_url(C)),
    Reason = atom_to_binary(wrong_masterkey, utf8),
    #'OperationAborted'{reason = Reason} = (catch cds_keyring_client:rotate(Id2, cds_crypto:sign(SigPrivateKey2, MasterKey2), root_url(C))).

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
    timer:sleep(Lifetime*1000 + Interval*2),
    ok = try
        _ = cds_card_client:get_session_card_data(Token, Session, root_url(C)),
        error
    catch
        throw:#'CardDataNotFound'{} ->
            ok
    end,
    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(Token, root_url(C)).

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
            ok = cds_maintenance:refresh_sessions_created_at(),
            timer:sleep(Lifetime * 100)
        end
    || _ <- lists:seq(1, 25)],

    timer:sleep(Interval),

    _ = cds_card_client:get_session_card_data(Token, Session, root_url(C)),

    timer:sleep(Lifetime * 1000 + Interval),

    ok = try
        _ = cds_card_client:get_session_card_data(Token, Session, root_url(C)),
        error
    catch
        throw:#'CardDataNotFound'{} ->
            ok
    end,
    ?CREDIT_CARD(<<>>) = cds_card_client:get_card_data(Token, root_url(C)).


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
    _ = timer:sleep(Interval * 3),
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
