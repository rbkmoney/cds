-module(cds_thrift_services).

-export([handler_spec/1]).
-export([service_path/1]).
-export([thrift_service/1]).
-export([handler_module/1]).

%%
%% Types
%%

-type storage_code() :: keyring_v2
                      | token
                      | card
                      | card_v2
                      | ident_doc.

-export_type([storage_code/0]).

%% Internal types

-type path() :: woody:path().
-type thrift_service() :: woody:service().
-type hadler_spec() :: woody:handler(list()).

-type service_hadler_spec() :: {path(), {thrift_service(), hadler_spec()}}.

%%
%% API
%%

-spec handler_spec(storage_code()) -> service_hadler_spec().
handler_spec(Code) ->
    {service_path(Code), {thrift_service(Code), handler_module(Code)}}.

-spec service_path(storage_code()) -> path().
service_path(token) ->
    "/v1/token_storage";
service_path(keyring_v2) ->
    "/v2/keyring";
service_path(card) ->
    "/v1/storage";
service_path(card_v2) ->
    "/v2/storage";
service_path(ident_doc) ->
    "/v1/identity_document_storage".

-spec thrift_service(storage_code()) -> thrift_service().
thrift_service(token) ->
    {tds_proto_storage_thrift, 'TokenStorage'};
thrift_service(keyring_v2) ->
    {cds_proto_keyring_thrift, 'Keyring'};
thrift_service(card) ->
    {dmsl_cds_thrift, 'Storage'};
thrift_service(card_v2) ->
    {cds_proto_storage_thrift, 'Storage'};
thrift_service(ident_doc) ->
    {identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}.

-spec handler_module(storage_code()) -> hadler_spec().
handler_module(token) ->
    {cds_token_thrift_handler, []};
handler_module(keyring_v2) ->
    {cds_keyring_v2_thrift_handler, []};
handler_module(card) ->
    {cds_card_v1_thrift_handler, []};
handler_module(card_v2) ->
    {cds_card_v2_thrift_handler, []};
handler_module(ident_doc) ->
    {cds_ident_doc_thrift_handler, []}.
