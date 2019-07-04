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
        token,
        fun() -> handle_function_(OperationID, Args, Context, Opts) end
    ).

handle_function_('GetToken', [TokenId], _Context, _opts) ->
    try
        TokenContent = cds_token_storage:get_token(TokenId),
        Token = #tds_Token{content = TokenContent},
        {ok, Token}
    catch
        not_found ->
            cds_thrift_handler_utils:raise(#tds_TokenNotFound{});
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Status)
    end;
handle_function_('PutToken', [TokenId, Token], _Context, _opts) ->
    try
        TokenContent = Token#tds_Token.content,
        ok = cds_token_storage:put_token(TokenId, TokenContent),
        {ok, ok}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise_keyring_unavailable(Status)
    end.
