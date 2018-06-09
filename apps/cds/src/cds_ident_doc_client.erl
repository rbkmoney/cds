-module(cds_ident_doc_client).

%% Identity document operations
-export([get_identity_doc/2]).
-export([put_identity_doc/2]).

%%
%% Internal Types
%%

-type result() :: cds_client_utils:result().
-type identity_doc() :: dmsl_identity_document_storage_thrift:'IdentityDocument'().

%%
%% API
%%

-spec get_identity_doc(cds:token(), woody:url()) -> result().
get_identity_doc(Token, RootUrl) ->
    cds_client_utils:call(identity_doc, 'Get', [Token], RootUrl).

-spec put_identity_doc(identity_doc(), woody:url()) -> result().
put_identity_doc(IdentityDoc, RootUrl) ->
    cds_client_utils:call(identity_doc, 'Put', [IdentityDoc], RootUrl).

%%
%% Internals
%%
