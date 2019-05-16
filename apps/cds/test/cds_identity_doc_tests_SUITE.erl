-module(cds_identity_doc_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").
-include_lib("eunit/include/eunit.hrl").

%% common_test callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests

-export([init/1]).
-export([put_passport/1]).
-export([put_insurance_cert/1]).
-export([assert_doc_stored/1]).

-spec test() -> _.

%%
%% Internal types
%%

-type config() :: [{atom(), any()}] | atom().

%%
%% common_test callbacks
%%

-spec all() -> [{group, atom()}].
all() ->
    [
        {group, riak_storage_backend},
        {group, ets_storage_backend}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].
groups() ->
    [
        {riak_storage_backend, [], [{group, general_flow}]},
        {ets_storage_backend, [], [{group, general_flow}]},
        {general_flow, [], [
            {group, basic_passport_lifecycle},
            {group, basic_insurance_cert_lifecycle}
        ]},
        {basic_passport_lifecycle, [sequence], [
            init,
            put_passport,
            assert_doc_stored
        ]},
        {basic_insurance_cert_lifecycle, [sequence], [
            init,
            put_insurance_cert,
            assert_doc_stored
        ]}
    ].

-spec init_per_group(atom(), config()) -> config().
init_per_group(riak_storage_backend, C) ->
    cds_ct_utils:set_riak_storage(C);
init_per_group(ets_storage_backend, C) ->
    cds_ct_utils:set_ets_storage(C);
init_per_group(Group, C) when
    Group =:= all;
    Group =:= general_flow
->
    C;
init_per_group(_, C) ->
    C1 = cds_ct_utils:start_stash(C),
    cds_ct_utils:start_clear(C1).

-spec end_per_group(atom(), config()) -> _.
end_per_group(Group, C) when
    Group =:= all;
    Group =:= ets_storage_backend;
    Group =:= riak_storage_backend;
    Group =:= general_flow
 ->
    C;
end_per_group(_, C) ->
    cds_ct_utils:stop_clear(C).

%%
%% Tests
%%


-spec init(config()) -> any() | no_return().
init(C) ->
    UMEncryptedMasterKeyShares = cds_keyring_client:start_init(2, root_url(C)),
    EncryptedMasterKeyShares =
        cds_keyring_thrift_handler:decode_encrypted_shares(UMEncryptedMasterKeyShares),
    _ = ?assertEqual(length(EncryptedMasterKeyShares), length(cds_shareholder:get_all())),
    EncPrivateKeys = enc_private_keys(C),
    SigPrivateKeys = sig_private_keys(C),
    DecryptedMasterKeyShares =
        cds_keyring_api_tests_SUITE:decrypt_and_sign_masterkeys(EncryptedMasterKeyShares, EncPrivateKeys, SigPrivateKeys),
    ok = cds_keyring_api_tests_SUITE:validate_init(DecryptedMasterKeyShares, C).

-spec put_passport(config()) -> any() | no_return().
put_passport(C) ->
    Doc = build_passport(),
    Token = cds_ident_doc_client:put_ident_doc(Doc, root_url(C)),
    true = erlang:is_binary(Token),
    ok = cds_ct_utils:store([{token, Token}, {doc, Doc}], C).

-spec put_insurance_cert(config()) -> any() | no_return().
put_insurance_cert(C) ->
    Doc = build_insurance_cert(),
    Token = cds_ident_doc_client:put_ident_doc(Doc, root_url(C)),
    true = erlang:is_binary(Token),
    ok = cds_ct_utils:store([{token, Token}, {doc, Doc}], C).

-spec assert_doc_stored(config()) -> any() | no_return().
assert_doc_stored(C) ->
    Token = cds_ct_utils:lookup(token, C),
    Doc = cds_ct_utils:lookup(doc, C),
    Doc = cds_ident_doc_client:get_ident_doc(Token, root_url(C)).

%%
%% Internals
%%

root_url(C) ->
    config(root_url, C).

enc_private_keys(C) ->
    config(enc_private_keys, C).

sig_private_keys(C) ->
    config(sig_private_keys, C).

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

build_passport() ->
    {russian_domestic_passport, #identdocstore_RussianDomesticPassport{
        series = <<"">>,
        number = <<"">>,
        issuer = <<"">>,
        issuer_code = <<"">>,
        issued_at = <<"">>,
        family_name = <<"">>,
        first_name = <<"">>,
        patronymic = <<"">>,
        birth_date = <<"">>,
        birth_place = <<"">>
    }}.

build_insurance_cert() ->
    {russian_retiree_insurance_certificate, #identdocstore_RussianRetireeInsuranceCertificate{
        number = <<"">>
    }}.
