-module(cds_ident_doc_client).

%% Identity document operations
-export([get_ident_doc/2]).
-export([put_ident_doc/2]).

%%
%% Internal Types
%%

-type result() :: cds_woody_client:result().
-type ident_doc() :: identdocstore_identity_document_storage_thrift:'IdentityDocument'().

%%
%% API
%%

-spec get_ident_doc(cds:token(), woody:url()) -> result().
get_ident_doc(Token, RootUrl) ->
    call(ident_doc, 'Get', [Token], RootUrl).

-spec put_ident_doc(ident_doc(), woody:url()) -> result().
put_ident_doc(IdentityDoc, RootUrl) ->
    call(ident_doc, 'Put', [IdentityDoc], RootUrl).

call(Service, Method, Args, RootUrl) ->
    cds_ct_utils:call(Service, Method, Args, RootUrl).
