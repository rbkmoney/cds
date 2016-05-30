-module(cds_client).

%% Storage operations
-export([get/1]).
-export([get_session/2]).
-export([put/1]).
-export([delete/1]).

%% Keyring operations
-export([init/2]).
-export([unlock/1]).
-export([lock/0]).
-export([rotate/0]).

-define(STATIC_CONTEXT, #{
    event_handler => cds_thrift_handler,
    parent_id => <<"undefined">>,
    root_rpc => true,
    rpc_id => undefined,
    seq => 0,
    span_id => <<"kek">>,
    trace_id => <<"kek">>
}).

get(Token) ->
    call('GetCardData', [Token]).

get_session(Token, Session) ->
    call('GetSessionCardData', [Token, Session]).

put(Data) ->
    call('PutCardData', [Data]).

delete(Token) ->
    call('DeleteCardData', [Token]).

init(Threshold, Number) ->
    call('Init', [Threshold, Number]).

unlock(Share) ->
    call('Unlock', [Share]).

lock() ->
    call('Lock', []).

rotate() ->
    call('Rotate', []).


call(Function, Args) ->
    Host = application:get_env(cds, thrift_client_host, "127.0.0.1"),
    Port = integer_to_list(application:get_env(cds, thrift_client_port, 8022)),
    Call = {{cds_cds_thrift, service(Function)}, Function, Args},
    Server = #{url => Host ++ ":" ++ Port ++ path(Function)},
    try woody_client:call(?STATIC_CONTEXT, Call, Server) of
        {ok, _Context} ->
            ok;
        {{ok, Response}, _Context} ->
            Response
    catch {{exception, Exception}, _Context} ->
        throw(Exception)
    end.

service('GetCardData') ->
    'Storage';
service('GetSessionCardData') ->
    'Storage';
service('PutCardData') ->
    'Storage';
service('DeleteCardData') ->
    'Keyring';
service('Init') ->
    'Keyring';
service('Unlock') ->
    'Keyring';
service('Lock') ->
    'Keyring';
service('Rotate') ->
    'Keyring'.

path('GetCardData') ->
    "/v1/storage";
path('GetSessionCardData') ->
    "/v1/storage";
path('PutCardData') ->
    "/v1/storage";
path('DeleteCardData') ->
    "/v1/keyring";
path('Init') ->
    "/v1/keyring";
path('Unlock') ->
    "/v1/keyring";
path('Lock') ->
    "/v1/keyring";
path('Rotate') ->
    "/v1/keyring".
