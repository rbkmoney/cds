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
    IP = application:get_env(cds, ip, "::1"),
    Port = integer_to_list(application:get_env(cds, port, 8022)),
    Request = {{cds_cds_thrift, service(Function)}, Function, Args},
    CallOpts = #{
        url => "http://" ++ "[" ++ IP ++ "]:" ++ Port ++ path(Function),
        event_handler => cds_thrift_handler
    },
    case woody_client:call(Request, CallOpts) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
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
