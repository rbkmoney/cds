-module(cds_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("tds_proto/include/tds_proto_storage_thrift.hrl").

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

filter_result({ok, Result}) -> {ok, filter(Result)};
filter_result({exception, Exception}) -> {exception, filter(Exception)};
filter_result(Result) -> filter(Result).

filter_args(Args) -> filter(Args).

filter(L) when is_list(L) -> [filter(E) || E <- L];
filter(M) when is_map(M) -> maps:map(fun (_K, V) -> filter(V) end, M);

filter(#'EncryptedMasterKeyShare'{} = EncryptedMasterKeyShare) ->
    EncryptedMasterKeyShare#'EncryptedMasterKeyShare'{encrypted_share = <<"***">>};
filter(#'SignedMasterKeyShare'{} = SignedShare) ->
    SignedShare#'SignedMasterKeyShare'{signed_share = <<"***">>};
filter(#'Keyring'{keys = Keys} = Keyring) ->
    Keyring#'Keyring'{keys = filter(Keys)};
filter(#'Key'{} = Key) ->
    Key#'Key'{data = <<"***">>};

filter(V) when is_integer(V) -> V;
filter(ok) -> ok;
filter({success, #'Success'{}} = V) -> V;
filter({more_keys_needed, D} = V) when is_integer(D) -> V;
filter(#'KeyringState'{} = V) -> V;
filter(#'KeyringMeta'{} = V) -> V;
filter(#'KeyringMetaDiff'{} = V) -> V;

%% cds storage

filter(#'CardData'{} = V) -> V;

%% tds
filter(#'tds_Token'{} = V) -> V;
filter(V) when is_bitstring(V) -> V;

filter(#'InvalidStatus'{} = V) -> V;
filter(#'InvalidActivity'{} = V) -> V;
filter(#'InvalidKeyringMeta'{} = V) -> V;
filter(#'InvalidArguments'{} = V) -> V;
filter(#'VerificationFailed'{} = V) -> V;
filter(#'OperationAborted'{} = V) -> V.
