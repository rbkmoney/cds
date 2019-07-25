-module(cds_keyring_client).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

-export([start_init/2]).
-export([validate_init/3]).
-export([cancel_init/1]).
-export([start_unlock/1]).
-export([confirm_unlock/3]).
-export([cancel_unlock/1]).
-export([lock/1]).
-export([start_rotate/1]).
-export([confirm_rotate/3]).
-export([cancel_rotate/1]).
-export([start_rekey/2]).
-export([confirm_rekey/3]).
-export([start_rekey_validation/1]).
-export([validate_rekey/3]).
-export([cancel_rekey/1]).
-export([get_state/1]).
-export([set_current_key/2]).

-type shareholder_id() :: binary().
-export_type([shareholder_id/0]).

-type encrypted_master_key_share() :: #{
    id := binary(),
    owner := binary(),
    encrypted_share := binary()
}.
-export_type([encrypted_master_key_share/0]).

-type masterkey_share() :: binary().
-export_type([masterkey_share/0]).

%%%
%%% Internal types
%%%

-type manager_state() :: locked | unlocked | not_initialized.
-type initializer_state() :: uninitialized | validation.
-type unlocker_state() :: uninitialized | validation.
-type rotator_state() :: uninitialized | validation.

%%%
%%% API
%%%

-spec start_init(integer(), woody:url()) ->
    [encrypted_master_key_share()] |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {initialization, initializer_state()}}} |
    {error, {invalid_arguments, binary()}}.
start_init(Threshold, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartInit', [Threshold], RootUrl) of
        EncryptedShares ->
            decode_encrypted_shares(EncryptedShares)
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_InvalidArguments{reason = Reason} ->
            {error, {invalid_arguments, Reason}}
    end.

-spec validate_init(shareholder_id(), masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {initialization, initializer_state()}}} |
    {error, verification_failed} |
    {error, {invalid_arguments, binary()}}.
validate_init(ShareholderId, Share, RootUrl) ->
    SignedShare = encode_signed_share(ShareholderId, Share),
    try cds_woody_client:call(keyring_v2, 'ValidateInit', [SignedShare], RootUrl) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_init(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {initialization, initializer_state()}}}.
cancel_init(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelInit', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec start_unlock(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {unlock, unlocker_state()}}}.
start_unlock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartUnlock', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec confirm_unlock(shareholder_id(), masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {unlock, unlocker_state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_unlock(ShareholderId, Share, RootUrl) ->
    SignedShare = encode_signed_share(ShareholderId, Share),
    try cds_woody_client:call(keyring_v2, 'ConfirmUnlock', [SignedShare], RootUrl) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_unlock(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}}.
cancel_unlock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelUnlock', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec lock(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}}.
lock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'Lock', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec start_rotate(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rotation, rotator_state()}}}.
start_rotate(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRotate', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec confirm_rotate(shareholder_id(), masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rotation, rotator_state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_rotate(ShareholderId, Share, RootUrl) ->
    SignedShare = encode_signed_share(ShareholderId, Share),
    try cds_woody_client:call(keyring_v2, 'ConfirmRotate', [SignedShare], RootUrl) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_rotate(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}}.
cancel_rotate(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelRotate', [], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec start_rekey(integer(), woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rekeying, rotator_state()}}} |
    {error, {invalid_arguments, binary()}}.
start_rekey(Threshold, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRekey', [Threshold], RootUrl) catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_InvalidArguments{reason = Reason} ->
            {error, {invalid_arguments, Reason}}
    end.

-spec confirm_rekey(shareholder_id(), masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rekeying, rotator_state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_rekey(ShareholderId, Share, RootUrl) ->
    SignedShare = encode_signed_share(ShareholderId, Share),
    try cds_woody_client:call(keyring_v2, 'ConfirmRekey', [SignedShare], RootUrl) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec start_rekey_validation(woody:url()) ->
    [encrypted_master_key_share()] |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rekeying, rotator_state()}}}.
start_rekey_validation(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRekeyValidation', [], RootUrl) of
        EncryptedShares ->
            decode_encrypted_shares(EncryptedShares)
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec validate_rekey(shareholder_id(), masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, manager_state()}} |
    {error, {invalid_activity, {rekeying, rotator_state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
validate_rekey(ShareholderId, Share, RootUrl) ->
    SignedShare = encode_signed_share(ShareholderId, Share),
    try cds_woody_client:call(keyring_v2, 'ValidateRekey', [SignedShare], RootUrl) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_rekey(woody:url()) ->
    ok |
    {error, {invalid_status, manager_state()}}.
cancel_rekey(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelRekey', [], RootUrl)
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec get_state(woody:url()) ->
    map(). % FIXME
get_state(RootUrl) ->
    State = cds_woody_client:call(keyring_v2, 'GetState', [], RootUrl),
    decode_state(State).

-spec set_current_key(cds_keyring:key_id(), woody:url()) ->
    ok | {error, {invalid_status, manager_state()} | {invalid_meta, binary()}}.

set_current_key(NewCurrentKeyID, RootUrl) ->
    try
        Diff = #cds_KeyringMetaDiff{current_key_id = NewCurrentKeyID},
        _ = cds_woody_client:call(keyring_v2, 'UpdateKeyringMeta', [Diff], RootUrl),
        ok
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidKeyringMeta{reason = Reason} ->
            {error, {invalid_meta, Reason}}
    end.

%%%
%%% Internal functions
%%%
encode_signed_share(ShareholderId, Share) ->
    #cds_SignedMasterKeyShare{
        id = ShareholderId,
        signed_share = Share
    }.

decode_state(#cds_KeyringState{
    status = Status,
    activities = #cds_ActivitiesState{
        initialization =  #cds_InitializationState{
            phase = InitPhase,
            lifetime = InitLifetime,
            validation_shares = InitValShares
        },
        unlock = #cds_UnlockState{
            phase = UnlockPhase,
            lifetime = UnlockLifetime,
            confirmation_shares = UnlockConShares
        },
        rotation = #cds_RotationState{
            phase = RotatePhase,
            lifetime = RotateLifetime,
            confirmation_shares = RotateConShares
        },
        rekeying = #cds_RekeyingState{
            phase = RekeyPhase,
            lifetime = RekeyLifetime,
            confirmation_shares = RekeyConShares,
            validation_shares = RekeyValShares
        }
    }
}) ->
    #{
        status => Status,
        activities => #{
            initialization => #{
                phase => InitPhase,
                lifetime => InitLifetime,
                validation_shares => InitValShares
            },
            unlock => #{
                phase => UnlockPhase,
                lifetime => UnlockLifetime,
                confirmation_shares => UnlockConShares
            },
            rotation => #{
                phase => RotatePhase,
                lifetime => RotateLifetime,
                confirmation_shares => RotateConShares
            },
            rekeying => #{
                phase => RekeyPhase,
                lifetime => RekeyLifetime,
                confirmation_shares => RekeyConShares,
                validation_shares => RekeyValShares
            }
        }
    }.

-spec decode_encrypted_shares([cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()]) ->
    [encrypted_master_key_share()].

decode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun decode_encrypted_share/1, EncryptedMasterKeyShares).

-spec decode_encrypted_share(cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()) ->
    encrypted_master_key_share().

decode_encrypted_share(#cds_EncryptedMasterKeyShare{
    id = Id,
    owner = Owner,
    encrypted_share = EncryptedShare
}) ->
    #{
        id => Id,
        owner => Owner,
        encrypted_share => EncryptedShare
    }.
