-module(cds_keyring_client).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

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

%%
%% Internal types
%%

-type encrypted_masterkey_share() :: dmsl_cds_thrift:'EncryptedMasterKeyShare'().

%%
%% API
%%

-spec start_init(integer(), woody:url()) ->
    [cds_keysharing:encrypted_master_key_share()] |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {initialization, cds_keyring_initializer:state()}}} |
    {error, {invalid_arguments, binary()}}.
start_init(Threshold, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartInit', [Threshold], RootUrl) of
        EncryptedShares ->
            decode_encrypted_shares(EncryptedShares)
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'InvalidArguments'{reason = Reason} ->
            {error, {invalid_arguments, Reason}}
    end.

-spec validate_init(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {initialization, cds_keyring_initializer:state()}}} |
    {error, verification_failed} |
    {error, {invalid_arguments, binary()}}.
validate_init(ShareholderId, Share, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'ValidateInit', [ShareholderId, Share], RootUrl) of
        {success, #'Success'{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'VerificationFailed'{} ->
            {error, verification_failed};
        #'OperationAborted'{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_init(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {initialization, cds_keyring_initializer:state()}}}.
cancel_init(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelInit', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec start_unlock(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {unlock, cds_keyring_unlocker:state()}}}.
start_unlock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartUnlock', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec confirm_unlock(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {unlock, cds_keyring_unlocker:state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_unlock(ShareholderId, Share, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'ConfirmUnlock', [ShareholderId, Share], RootUrl) of
        {success, #'Success'{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'VerificationFailed'{} ->
            {error, verification_failed};
        #'OperationAborted'{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_unlock(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}}.
cancel_unlock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelUnlock', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec lock(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}}.
lock(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'Lock', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec start_rotate(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rotation, cds_keyring_rotator:state()}}}.
start_rotate(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRotate', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec confirm_rotate(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rotation, cds_keyring_rotator:state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_rotate(ShareholderId, Share, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'ConfirmRotate', [ShareholderId, Share], RootUrl) of
        {success, #'Success'{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'VerificationFailed'{} ->
            {error, verification_failed};
        #'OperationAborted'{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_rotate(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}}.
cancel_rotate(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelRotate', [], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec start_rekey(integer(), woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rekeying, cds_keyring_rotator:state()}}} |
    {error, {invalid_arguments, binary()}}.
start_rekey(Threshold, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRekey', [Threshold], RootUrl) catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'InvalidArguments'{reason = Reason} ->
            {error, {invalid_arguments, Reason}}
    end.

-spec confirm_rekey(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rekeying, cds_keyring_rotator:state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
confirm_rekey(ShareholderId, Share, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'ConfirmRekey', [ShareholderId, Share], RootUrl) of
        {success, #'Success'{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'VerificationFailed'{} ->
            {error, verification_failed};
        #'OperationAborted'{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec start_rekey_validation(woody:url()) ->
    [cds_keysharing:encrypted_master_key_share()] |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rekeying, cds_keyring_rotator:state()}}}.
start_rekey_validation(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'StartRekeyValidation', [], RootUrl) of
        EncryptedShares ->
            decode_encrypted_shares(EncryptedShares)
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}}
    end.

-spec validate_rekey(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) ->
    ok | {more_keys_needed, non_neg_integer()} |
    {error, {invalid_status, cds_keyring_manager:state()}} |
    {error, {invalid_activity, {rekeying, cds_keyring_rotator:state()}}} |
    {error, verification_failed} |
    {error, {operation_aborted, binary()}}.
validate_rekey(ShareholderId, Share, RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'ValidateRekey', [ShareholderId, Share], RootUrl) of
        {success, #'Success'{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}};
        #'InvalidActivity'{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #'VerificationFailed'{} ->
            {error, verification_failed};
        #'OperationAborted'{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

-spec cancel_rekey(woody:url()) ->
    ok |
    {error, {invalid_status, cds_keyring_manager:state()}}.
cancel_rekey(RootUrl) ->
    try cds_woody_client:call(keyring_v2, 'CancelRekey', [], RootUrl)
    catch
        #'InvalidStatus'{status = Status} ->
            {error, {invalid_status, Status}}
    end.

-spec get_state(woody:url()) -> cds_keyring_manager:status().
get_state(RootUrl) ->
    State = cds_woody_client:call(keyring_v2, 'GetState', [], RootUrl),
    decode_state(State).

decode_state(#'KeyringState'{
    status = Status,
    activities = #'ActivitiesState'{
        initialization =  #'InitializationState'{
            phase = InitPhase,
            lifetime = InitLifetime,
            validation_shares = InitValShares
        },
        unlock = #'UnlockState'{
            phase = UnlockPhase,
            lifetime = UnlockLifetime,
            confirmation_shares = UnlockConShares
        },
        rotation = #'RotationState'{
            phase = RotatePhase,
            lifetime = RotateLifetime,
            confirmation_shares = RotateConShares
        },
        rekeying = #'RekeyingState'{
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

-spec decode_encrypted_shares([encrypted_masterkey_share()]) ->
    [cds_keysharing:encrypted_master_key_share()].

decode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun decode_encrypted_share/1, EncryptedMasterKeyShares).

-spec decode_encrypted_share(encrypted_masterkey_share()) ->
    cds_keysharing:encrypted_master_key_share().

decode_encrypted_share(#'EncryptedMasterKeyShare' {
    id = Id,
    owner = Owner,
    encrypted_share = EncryptedShare
}) ->
    #{
        id => Id,
        owner => Owner,
        encrypted_share => EncryptedShare
    }.