-module(cds_token_client).

%% Token operations
-export([get_token/2]).
-export([put_token/3]).

%%
%% Internal Types
%%

-type result() :: cds_woody_client:result().
-type token() :: tds_proto_storage_thrift:'Token'().
-type token_id() :: tds_proto_storage_thrift:'TokenID'().

%%
%% API
%%

-spec get_token(token_id(), woody:url()) -> result().
get_token(TokenId, RootUrl) ->
    cds_woody_client:call(token, 'GetToken', {TokenId}, RootUrl).

-spec put_token(token_id(), token(), woody:url()) -> result().
put_token(TokenId, Token, RootUrl) ->
    cds_woody_client:call(token, 'PutToken', {TokenId, Token}, RootUrl).
