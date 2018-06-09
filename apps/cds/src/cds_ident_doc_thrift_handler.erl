-module(cds_ident_doc_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_identity_document_storage_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('Put', [IdentityDocument], _Context, _Opts) ->
    Doc = decode(IdentityDocument),
    try
        Token = cds_ident_doc_storage:put_identity_document(Doc),
        {ok, Token}
    catch
        Reason when Reason == locked; Reason == not_initialized ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Reason)
    end;
handle_function('Get', [IdentityDocumentToken], _Context, _Opts) ->
    try
        Doc = cds_ident_doc_storage:get_identity_document(IdentityDocumentToken),
        {ok, encode(Doc)}
    catch
        not_found ->
            cds_thrift_handler_utils:raise(#ident_doc_store_IdentityDocumentNotFound{});
        Reason when Reason == locked; Reason == not_initialized ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Reason)
    end.

%%
%% Internals
%%

-spec decode(dmsl_identity_document_storage_thrift:'IdentityDocument'()) ->
    cds_ident_doc:identity_document().
decode({
    russian_domestic_passport,
    #ident_doc_store_RussianDomesticPassport{
        series = Series,
        number = Number,
        issuer = Issuer,
        issuer_code = IssuerCode,
        issued_at = IssuedAt,
        family_name = FamilyName,
        first_name = FirstName,
        patronymic = Patronymic,
        birth_date = BirthDate,
        birth_place = BirthPlace
    }
}) ->
    #{
        type => russian_domestic_passport,
        series => Series,
        number => Number,
        issuer => Issuer,
        issuer_code => IssuerCode,
        issued_at => IssuedAt,
        family_name => FamilyName,
        first_name => FirstName,
        patronymic => Patronymic,
        birth_date => BirthDate,
        birth_place => BirthPlace
    };
decode({
    russian_retiree_insurance_certificate,
    #ident_doc_store_RussianRetireeInsuranceCertificate{
        number = Number
    }
}) ->
    #{
        type => russian_retiree_insurance_certificate,
        number => Number
    }.

-spec encode(cds_ident_doc:identity_document()) ->
    dmsl_identity_document_storage_thrift:'IdentityDocument'().
encode(#{type := russian_domestic_passport} = Doc) ->
    {russian_domestic_passport, encode_russian_domestic_passport(Doc)};
encode(#{type := russian_retiree_insurance_certificate} = Doc) ->
    {russian_retiree_insurance_certificate, encode_russian_retiree_insurance_certificate(Doc)}.

encode_russian_domestic_passport(Doc) ->
    #ident_doc_store_RussianDomesticPassport{
        series = maps:get(series, Doc),
        number = maps:get(number, Doc),
        issuer = maps:get(issuer, Doc),
        issuer_code = maps:get(issuer_code, Doc),
        issued_at = maps:get(issued_at, Doc),
        family_name = maps:get(family_name, Doc),
        first_name = maps:get(first_name, Doc),
        patronymic = maps:get(patronymic, Doc),
        birth_date = maps:get(birth_date, Doc),
        birth_place = maps:get(birth_place, Doc)
    }.

encode_russian_retiree_insurance_certificate(Doc) ->
    #ident_doc_store_RussianRetireeInsuranceCertificate{
        number = maps:get(number, Doc)
    }.
