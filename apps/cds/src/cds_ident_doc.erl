-module(cds_ident_doc).

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

-export_type([identity_document/0]).

-export([marshal/1]).
-export([unmarshal/1]).

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
