-module(cds_client).

%% Storage operations
-export([get/2]).
-export([get_session/3]).
-export([put/2]).
-export([delete/2]).

%% Keyring operations
-export([init/3]).
-export([unlock/2]).
-export([lock/1]).
-export([rotate/1]).

get(Token, RootUrl) ->
    call('GetCardData', [Token], RootUrl).

get_session(Token, Session, RootUrl) ->
    call('GetSessionCardData', [Token, Session], RootUrl).

put(Data, RootUrl) ->
    call('PutCardData', [Data], RootUrl).

delete(Token, RootUrl) ->
    call('DeleteCardData', [Token], RootUrl).

init(Threshold, Number, RootUrl) ->
    call('Init', [Threshold, Number], RootUrl).

unlock(Share, RootUrl) ->
    call('Unlock', [Share], RootUrl).

lock(RootUrl) ->
    call('Lock', [], RootUrl).

rotate(RootUrl) ->
    call('Rotate', [], RootUrl).

call(Function, Args, RootUrl) ->
    Request = {{cds_cds_thrift, service(Function)}, Function, Args},
    CallOpts = #{
        url => RootUrl ++ path(Function),
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
