-module(cds_keyring_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).
-export([decode_encrypted_shares/1]).

-type encrypted_masterkey_share() :: #'EncryptedMasterKeyShare' {}.

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(OperationID, Args, Context, Opts) ->
    scoper:scope(
        keyring,
        fun() -> handle_function_(OperationID, Args, Context, Opts) end
    ).

handle_function_('StartInit', [Threshold], _Context, _Opts) ->
    try cds_keyring_manager:initialize(Threshold) of
        EncryptedMasterKeyShares ->
            {ok, encode_encrypted_shares(EncryptedMasterKeyShares)}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {invalid_activity, Activity} ->
            cds_thrift_handler_utils:raise(#'InvalidActivity'{activity = Activity});
        invalid_args ->
            cds_thrift_handler_utils:raise(#'InvalidArguments'{})
    end;
handle_function_('ValidateInit', [Share], _Context, _Opts) ->
    try cds_keyring_manager:validate_init(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {invalid_activity, Activity} ->
            cds_thrift_handler_utils:raise(#'InvalidActivity'{activity = Activity});
        {operation_aborted, Reason} ->
            cds_thrift_handler_utils:raise(#'OperationAborted'{reason = atom_to_binary(Reason, utf8)})
    end;
handle_function_('CancelInit', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:cancel_init()} catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status})
    end;
handle_function_('Lock', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:lock()} catch
        {invalid_status, locked} ->
            {ok, ok};
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status})
    end;
handle_function_('Unlock', [Share], _Context, _Opts) ->
    try cds_keyring_manager:unlock(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status})
    end;
handle_function_('Rotate', [Share], _Context, _Opts) ->
    try cds_keyring_manager:rotate(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {operation_aborted, Reason} ->
            cds_thrift_handler_utils:raise(#'OperationAborted'{reason = atom_to_binary(Reason, utf8)})
    end.

-spec encode_encrypted_shares([cds_keyring_utils:encrypted_master_key_share()]) ->
    [encrypted_masterkey_share()].

encode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun encode_encrypted_share/1, EncryptedMasterKeyShares).

-spec encode_encrypted_share(cds_keyring_utils:encrypted_master_key_share()) ->
    encrypted_masterkey_share().

encode_encrypted_share(#{
    id := Id,
    owner := Owner,
    encrypted_share := EncryptedShare
}) ->
  #'EncryptedMasterKeyShare' {
      id = Id,
      owner = Owner,
      encrypted_share = EncryptedShare
  }.

-spec decode_encrypted_shares([encrypted_masterkey_share()]) ->
    [cds_keyring_utils:encrypted_master_key_share()].

decode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun decode_encrypted_share/1, EncryptedMasterKeyShares).

-spec decode_encrypted_share(encrypted_masterkey_share()) ->
    cds_keyring_utils:encrypted_master_key_share().

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