-module(cds_ident_doc_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(OperationID, Args, Context, Opts) ->
    scoper:scope(
        ident_doc,
        fun() ->
            try
                handle_function_(OperationID, Args, Context, Opts)
            catch
                throw:Exception ->
                    throw(Exception);
                error:{woody_error, _} = WoodyError ->
                    erlang:error(WoodyError);
                _Class:_Exception:Stacktrace ->
                    woody_error:raise(system, {internal, result_unexpected, Stacktrace})
            end
        end
    ).

handle_function_('Put', [IdentityDocument], _Context, _Opts) ->
    Doc = decode(IdentityDocument),
    try
        Token = cds_ident_doc_storage:put_identity_document(Doc),
        EToken = cds_utils:encode_token(Token),
        {ok, EToken}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Status)
    end;
handle_function_('Get', [IdentityDocumentToken], _Context, _Opts) ->
    try
        DToken = cds_utils:decode_token(IdentityDocumentToken),
        Doc = cds_ident_doc_storage:get_identity_document(DToken),
        {ok, encode(Doc)}
    catch
        not_found ->
            cds_thrift_handler_utils:raise(#identdocstore_IdentityDocumentNotFound{});
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Status)
    end.

%%
%% Internals
%%

-spec decode(identdocstore_identity_document_storage_thrift:'IdentityDocument'()) ->
    cds_ident_doc:identity_document().
decode({
    russian_domestic_passport,
    #identdocstore_RussianDomesticPassport{
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
    #identdocstore_RussianRetireeInsuranceCertificate{
        number = Number
    }
}) ->
    #{
        type => russian_retiree_insurance_certificate,
        number => Number
    }.

-spec encode(cds_ident_doc:identity_document()) ->
    identdocstore_identity_document_storage_thrift:'IdentityDocument'().
encode(#{type := russian_domestic_passport} = Doc) ->
    {russian_domestic_passport, encode_russian_domestic_passport(Doc)};
encode(#{type := russian_retiree_insurance_certificate} = Doc) ->
    {russian_retiree_insurance_certificate, encode_russian_retiree_insurance_certificate(Doc)}.

encode_russian_domestic_passport(Doc) ->
    #identdocstore_RussianDomesticPassport{
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
    #identdocstore_RussianRetireeInsuranceCertificate{
        number = maps:get(number, Doc)
    }.
