-module(cds_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, RawMeta, Opts) ->
    FilteredMeta = filter_meta(RawMeta),
    scoper_woody_event_handler:handle_event(Event, RpcID, FilteredMeta, Opts).

filter_meta(RawMeta) ->
    case RawMeta of
        #{result := Result} ->
            RawMeta#{result => filter_result(Result)};
        #{args := Args} ->
            RawMeta#{args => filter_args(Args)};
        _ ->
            RawMeta
    end.

filter_result(Result) ->
    case Result of
        [#'EncryptedMasterKeyShare'{} | _Rest] = EncryptedMasterKeyShares ->
            filter_encrypted_master_key_shares(EncryptedMasterKeyShares);
        {ok, [#'EncryptedMasterKeyShare'{} | _Rest] = EncryptedMasterKeyShares} ->
            filter_encrypted_master_key_shares(EncryptedMasterKeyShares);
        #'Keyring'{keys = Keys} = Keyring ->
            Keyring#'Keyring'{keys = filter_keys(Keys)};
        {ok, #'Keyring'{keys = Keys} = Keyring} ->
            Keyring#'Keyring'{keys = filter_keys(Keys)};
        _ ->
            Result
    end.

filter_args(Args) ->
    case Args of
        [#'SignedMasterKeyShare'{} = SignedShare] ->
            [SignedShare#'SignedMasterKeyShare'{signed_share = <<"***">>}];
        _ ->
            Args
    end.

filter_encrypted_master_key_shares(EncryptedMasterKeyShares) ->
    lists:map(
        fun (EncryptedMasterKeyShare) ->
            EncryptedMasterKeyShare#'EncryptedMasterKeyShare'{encrypted_share = <<"***">>}
        end,
        EncryptedMasterKeyShares).

filter_keys(Keys) ->
    maps:map(fun (_K, #'Key'{} = Key) -> Key#'Key'{data = <<"***">>} end, Keys).
