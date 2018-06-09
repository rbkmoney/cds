-module(cds_thrift_services).

-export([handler_spec/1]).
-export([service_path/1]).
-export([thrift_service/1]).
-export([handler_module/1]).

%%
%% Types
%%

-type storage_code() :: keyring
    | card
    | identity_doc.

-export_type([storage_code/0]).

%% Internal types

-type path() :: woody:path().
-type thrift_service() :: woody:service().
-type hadler_spec() :: woody:handler().

-type service_hadler_spec() :: {path(), {thrift_service(), hadler_spec()}}.

%%
%% API
%%

-spec handler_spec(storage_code()) -> service_hadler_spec().
handler_spec(Code) ->
    {service_path(Code), {thrift_service(Code), handler_module(Code)}}.

-spec service_path(storage_code()) -> path().
service_path(keyring) ->
    "/v1/keyring";
service_path(card) ->
    "/v1/storage";
service_path(identity_doc) ->
    "/v1/identity_document_storage".

-spec thrift_service(storage_code()) -> thrift_service().
thrift_service(keyring) ->
    {dmsl_cds_thrift, 'Keyring'};
thrift_service(card) ->
    {dmsl_cds_thrift, 'Storage'};
thrift_service(identity_doc) ->
    {dmsl_identity_document_storage_thrift, 'IdentityDocumentStorage'}.

-spec handler_module(storage_code()) -> hadler_spec().
handler_module(keyring) ->
    {cds_keyring_thrift_handler, []};
handler_module(card) ->
    {cds_card_thrift_handler, []};
handler_module(identity_doc) ->
    {cds_ident_doc_thrift_handler, []}.
