-module(cds_token_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("tds_proto/include/tds_proto_storage_thrift.hrl").

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
        fun() -> handle_function_(OperationID, Args, Context, Opts) end
    ).

% TODO: implementation
handle_function_('GetToken', [_TokenId], _Context, _opts) ->
    Token = #tds_Token{content = <<"kek">>},
    {ok, Token};
handle_function_('PutToken', [_TokenId, _Token], _Context, _opts) ->
    {ok, ok}.
