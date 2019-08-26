-module(cds_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("tds_proto/include/tds_proto_storage_thrift.hrl").
-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").
-include_lib("woody/src/woody_defs.hrl").

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

filter_meta(RawMeta0) ->
    RawMeta1 = filter_meta_result(RawMeta0),
    RawMeta2 = filter_meta_args(RawMeta1),
    RawMeta2.

filter_meta_result(#{result := Result} = Meta) ->
    Meta#{result => filter(Result)};
filter_meta_result(#{reason := Error} = Meta) ->
    Meta#{reason => filter(Error)};
filter_meta_result(Meta) ->
    Meta.

filter_meta_args(#{args := Args} = Meta) ->
    Meta#{args => filter(Args)};
filter_meta_args(Meta) ->
    Meta.

%% woody errors
filter({internal, Error, Details} = V) when is_atom(Error) and is_binary(Details) -> V;
filter({external, Error, Details} = V) when is_atom(Error) and is_binary(Details) -> V;

%% cds_proto
filter(#cds_EncryptedMasterKeyShare{} = EncryptedMasterKeyShare) ->
    EncryptedMasterKeyShare#cds_EncryptedMasterKeyShare{encrypted_share = <<"***">>};
filter(#cds_SignedMasterKeyShare{} = SignedShare) ->
    SignedShare#cds_SignedMasterKeyShare{signed_share = <<"***">>};
filter(#cds_Keyring{keys = Keys} = Keyring) ->
    Keyring#cds_Keyring{keys = filter(Keys)};
filter(#cds_Key{} = Key) ->
    Key#cds_Key{data = <<"***">>};
filter({success, #cds_Success{}} = V) -> V;
filter({more_keys_needed, D} = V) when is_integer(D) -> V;
filter(#cds_KeyringState{} = V) -> V;
filter(#cds_KeyringMeta{} = V) -> V;
filter(#cds_KeyringMetaDiff{} = V) -> V;

%% cds_proto storage
filter(#cds_CardData{} = V) -> V;
filter(#cds_SessionData{} = V) -> V;
filter(#cds_PutCardResult{} = V) -> V;
filter(#cds_PutCardDataResult{} = V) -> V;

%% damsel storage
filter(#'CardData'{} = V) -> V;
filter(#'SessionData'{} = V) -> V;
filter(#'PutCardResult'{} = V) -> V;
filter(#'PutCardDataResult'{} = V) -> V;

%% identdocstore
filter({russian_domestic_passport, #identdocstore_RussianDomesticPassport{}}) ->
    FilteredPassport = #identdocstore_RussianDomesticPassport{
        series = <<"***">>,
        number = <<"***">>,
        issuer = <<"***">>,
        issuer_code = <<"***">>,
        issued_at = <<"***">>,
        family_name = <<"***">>,
        first_name = <<"***">>,
        patronymic = <<"***">>,
        birth_date = <<"***">>,
        birth_place = <<"***">>
    },
    {russian_domestic_passport, FilteredPassport};
filter({russian_retiree_insurance_certificate, #identdocstore_RussianRetireeInsuranceCertificate{}} = V) -> V;

%% tds
filter(#tds_Token{} = V) -> V#tds_Token{content = <<"***">>};

%% cds_proto exceptions
filter(#cds_InvalidStatus{} = V) -> V;
filter(#cds_InvalidActivity{} = V) -> V;
filter(#cds_InvalidKeyringMeta{} = V) -> V;
filter(#cds_InvalidArguments{} = V) -> V;
filter(#cds_VerificationFailed{} = V) -> V;
filter(#cds_OperationAborted{} = V) -> V;
filter(#cds_InvalidCardData{} = V) -> V;
filter(#cds_CardDataNotFound{} = V) -> V;
filter(#cds_SessionDataNotFound{} = V) -> V;

%% damsel exceptions
filter(#'InvalidCardData'{} = V) -> V;
filter(#'CardDataNotFound'{} = V) -> V;
filter(#'SessionDataNotFound'{} = V) -> V;

%% identdocstore exceptions
filter(#identdocstore_IdentityDocumentNotFound{} = V) -> V;

%% tds exceptions
filter(#tds_TokenNotFound{} = V) -> V;

%% common
filter(V) when is_atom(V) -> V;
filter(V) when is_number(V) -> V;
filter(L) when is_list(L) -> [filter(E) || E <- L];
filter(T) when is_tuple(T) -> list_to_tuple(filter(tuple_to_list(T)));
filter(M) when is_map(M) -> maps:map(fun (_K, V) -> filter(V) end, M);
filter(B) when is_binary(B) -> <<"***">>;
filter(B) when is_bitstring(B) -> <<"***">>;
filter(P) when is_pid(P) -> P;
filter(R) when is_reference(R) -> R;

%% fallback
filter(_V) -> <<"*filtered*">>.
