-module(cds_identity_document).

-include_lib("dmsl/include/dmsl_identity_document_storage_thrift.hrl").

-type identity_document() :: russian_domestic_passport() | russian_retiree_insurance_certificate().
-type russian_domestic_passport() :: #{
    type := russian_domestic_passport,
    series := binary(),
    number := binary(),
    issuer := binary(),
    issuer_code := binary(),
    issued_at := binary(),
    family_name := binary(),
    first_name := binary(),
    patronymic := binary(),
    birth_date := binary(),
    birth_place := binary()
}.
-type russian_retiree_insurance_certificate() :: #{
    type := russian_retiree_insurance_certificate,
    number := binary()
}.
-type safe_identity_document() :: safe_russian_domestic_passport() | safe_russian_retiree_insurance_certificate().
-type safe_russian_domestic_passport() :: #{
    type := safe_russian_domestic_passport,
    series := binary(),
    number := binary(),
    fullname := binary()
}.
-type safe_russian_retiree_insurance_certificate() :: #{
    type := safe_russian_retiree_insurance_certificate,
    number := binary()
}.

-export_type([identity_document/0]).
-export_type([safe_identity_document/0]).

-export([decode/1]).
-export([encode/1]).
-export([get_safe_data/1]).
-export([encode_safe_data/1]).

-export([marshal/1]).
-export([unmarshal/1]).

-spec decode(dmsl_identity_document_storage_thrift:'IdentityDocument'()) ->
    identity_document().

decode({
    russian_domestic_passport,
    #id_storage_RussianDomesticPassport{
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
    #id_storage_RussianRetireeInsuranceCertificate{
        number = Number
    }
}) ->
    #{
        type => russian_retiree_insurance_certificate,
        number => Number
    }.

-spec encode(identity_document()) ->
    dmsl_identity_document_storage_thrift:'IdentityDocument'().

encode(#{type := russian_domestic_passport} = Doc) ->
    {russian_domestic_passport, encode_russian_domestic_passport(Doc)};
encode(#{type := russian_retiree_insurance_certificate} = Doc) ->
    {russian_retiree_insurance_certificate, encode_russian_retiree_insurance_certificate(Doc)}.

-spec get_safe_data(identity_document()) ->
    safe_identity_document().

%% TODO implement this
get_safe_data(#{type := russian_domestic_passport} = Doc) ->
    #{
        type => safe_russian_domestic_passport,
        series => maps:get(series, Doc),
        number => maps:get(number, Doc),
        fullname => maps:get(first_name, Doc)
    };
get_safe_data(#{type := russian_retiree_insurance_certificate} = Doc) ->
    #{
        type => safe_russian_retiree_insurance_certificate,
        number => maps:get(number, Doc)
    }.

-spec encode_safe_data({binary(), safe_identity_document()}) ->
    dmsl_identity_document_storage_thrift:'SafeIdentityDocumentData'().

encode_safe_data({Token, SafeIdentityDocument}) ->
    #id_storage_SafeIdentityDocumentData{
        token = Token,
        safe_identity_document = encode_safe_identity_document(SafeIdentityDocument)
    }.

-spec marshal(identity_document()) ->
    binary().

marshal(Doc) ->
    msgpack:pack(marshal_(Doc)).

-spec unmarshal(binary()) ->
    identity_document().

unmarshal(Bin) ->
    {ok, Doc} = msgpack:unpack(Bin),
    unmarshal_(Doc).

%%
%% Internals
%%

encode_russian_domestic_passport(Doc) ->
    #id_storage_RussianDomesticPassport{
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
    #id_storage_RussianRetireeInsuranceCertificate{
        number = maps:get(number, Doc)
    }.

encode_safe_identity_document(#{type := safe_russian_domestic_passport} = Doc) ->
    {safe_russian_domestic_passport, encode_safe_russian_domestic_passport(Doc)};
encode_safe_identity_document(#{type := safe_russian_retiree_insurance_certificate} = Doc) ->
    {safe_russian_retiree_insurance_certificate, encode_safe_russian_retiree_insurance_certificate(Doc)}.

encode_safe_russian_domestic_passport(Doc) ->
    #id_storage_SafeRussianDomesticPassport{
        series_masked = maps:get(series, Doc),
        number_masked = maps:get(number, Doc),
        fullname_masked = maps:get(fullname, Doc)
    }.

encode_safe_russian_retiree_insurance_certificate(Doc) ->
    #id_storage_SafeRussianRetireeInsuranceCertificate{
        number_masked = maps:get(number, Doc)
    }.

marshal_(#{type := russian_domestic_passport} = Doc) ->
    genlib_map:compact(#{
        <<"type">> => <<"russian_domestic_passport">>,
        <<"series">> => maps:get(series, Doc),
        <<"number">> => maps:get(number, Doc),
        <<"issuer">> => maps:get(issuer, Doc),
        <<"issuer_code">> => maps:get(issuer_code, Doc),
        <<"issued_at">> => maps:get(issued_at, Doc),
        <<"family_name">> => maps:get(family_name, Doc),
        <<"first_name">> => maps:get(first_name, Doc),
        <<"patronymic">> => maps:get(patronymic, Doc),
        <<"birth_date">> => maps:get(birth_date, Doc),
        <<"birth_place">> => maps:get(birth_place, Doc)
    });
marshal_(#{type := russian_retiree_insurance_certificate} = Doc) ->
    #{
        <<"type">> => <<"russian_retiree_insurance_certificate">>,
        <<"number">> => maps:get(number, Doc)
    }.

unmarshal_(#{<<"type">> := <<"russian_domestic_passport">>} = Doc) ->
    #{
        type => russian_domestic_passport,
        series => maps:get(<<"series">>, Doc),
        number => maps:get(<<"number">>, Doc),
        issuer => maps:get(<<"issuer">>, Doc),
        issuer_code => maps:get(<<"issuer_code">>, Doc),
        issued_at => maps:get(<<"issued_at">>, Doc),
        family_name => maps:get(<<"family_name">>, Doc),
        first_name => maps:get(<<"first_name">>, Doc),
        patronymic => maps:get(<<"patronymic">>, Doc, undefined),
        birth_date => maps:get(<<"birth_date">>, Doc),
        birth_place => maps:get(<<"birth_place">>, Doc)
    };
unmarshal_(#{<<"type">> := <<"russian_retiree_insurance_certificate">>} = Doc) ->
    #{
        type => russian_retiree_insurance_certificate,
        number => maps:get(<<"number">>, Doc)
    }.
