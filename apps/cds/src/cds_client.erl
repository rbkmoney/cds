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

-type result() :: woody:result() |
    {exception, woody_error:business_error()} |
    no_return().

-spec get(_, woody:url()) -> result().

get(Token, RootUrl) ->
    call('GetCardData', [Token], RootUrl).

-spec get_session(_, _, woody:url()) -> result().

get_session(Token, Session, RootUrl) ->
    call('GetSessionCardData', [Token, Session], RootUrl).

-spec put(_, woody:url()) -> result().

put(Data, RootUrl) ->
    call('PutCardData', [Data], RootUrl).

-spec delete(_, woody:url()) -> result().

delete(Token, RootUrl) ->
    call('DeleteCardData', [Token], RootUrl).

-spec init(_, _, woody:url()) -> result().

init(Threshold, Number, RootUrl) ->
    call('Init', [Threshold, Number], RootUrl).

-spec unlock(_, woody:url()) -> result().

unlock(Share, RootUrl) ->
    call('Unlock', [Share], RootUrl).

-spec lock(woody:url()) -> result().

lock(RootUrl) ->
    call('Lock', [], RootUrl).

-spec rotate(woody:url()) -> result().

rotate(RootUrl) ->
    call('Rotate', [], RootUrl).

-spec call(atom(), list(), woody:url()) -> result().

call(Function, Args, RootUrl) ->
    Request = {{cds_cds_thrift, service(Function)}, Function, Args},
    Path = genlib:to_binary(path(Function)),
    CallOpts = #{
        url => <<RootUrl/binary, Path/binary>>,
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
