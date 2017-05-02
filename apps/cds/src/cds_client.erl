-module(cds_client).

%% Storage operations
-export([get_card_data/2]).
-export([get_session_card_data/3]).
-export([put_card_data/2]).

%% Keyring operations
-export([init/3]).
-export([unlock/2]).
-export([lock/1]).
-export([rotate/1]).

-type result() :: woody:result() |
    {exception, woody_error:business_error()} |
    no_return().

-spec get_card_data(cds:token(), woody:url()) -> result().

get_card_data(Token, RootUrl) ->
    call('GetCardData', [Token], RootUrl).

-spec get_session_card_data(cds:token(), cds:session(), woody:url()) -> result().

get_session_card_data(Token, Session, RootUrl) ->
    call('GetSessionCardData', [Token, Session], RootUrl).

-spec put_card_data(cds_cds_thrift:'CardData'(), woody:url()) -> result().

put_card_data(Data, RootUrl) ->
    call('PutCardData', [Data], RootUrl).

-spec init(integer(), integer(), woody:url()) -> result().

init(Threshold, Number, RootUrl) ->
    call('Init', [Threshold, Number], RootUrl).

-spec unlock(cds:masterkey_share(), woody:url()) -> result().

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
path('Init') ->
    "/v1/keyring";
path('Unlock') ->
    "/v1/keyring";
path('Lock') ->
    "/v1/keyring";
path('Rotate') ->
    "/v1/keyring".
