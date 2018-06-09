-module(cds_client).

%% Storage operations
-export([get_card_data/2]).
-export([get_session_card_data/3]).
-export([put_card_data/2]).
-export([put_card_data/3]).
-export([get_session_data/2]).

%% Keyring operations
-export([init/3]).
-export([unlock/2]).
-export([lock/1]).
-export([rotate/1]).

%% Identity document operations
-export([get_identity_doc/2]).
-export([put_identity_doc/2]).

%%
%% Internal Types
%%

-type result() :: woody:result()
    | {exception, woody_error:business_error()}
    | no_return().
-type card_data() :: dmsl_cds_thrift:'CardData'().
-type session_data() :: dmsl_cds_thrift:'SessionData'() | undefined.
-type identity_doc() :: dmsl_identity_document_storage_thrift:'IdentityDocument'().

%%
%% API
%%

%% Storage operations

-spec get_card_data(cds:token(), woody:url()) -> result().
get_card_data(Token, RootUrl) ->
    call(card, 'GetCardData', [Token], RootUrl).

-spec get_session_card_data(cds:token(), cds:session(), woody:url()) -> result().
get_session_card_data(Token, Session, RootUrl) ->
    call(card, 'GetSessionCardData', [Token, Session], RootUrl).

-spec put_card_data(card_data(), woody:url()) -> result().
put_card_data(CardData, RootUrl) ->
    put_card_data(CardData, undefined, RootUrl).

-spec put_card_data(card_data(), session_data(), woody:url()) -> result().
put_card_data(CardData, SessionData, RootUrl) ->
    call(card, 'PutCardData', [CardData, SessionData], RootUrl).

-spec get_session_data(cds:session(), woody:url()) -> result().
get_session_data(Session, RootUrl) ->
    call(card, 'GetSessionData', [Session], RootUrl).

%% Keyring operations

-spec init(integer(), integer(), woody:url()) -> result().
init(Threshold, Number, RootUrl) ->
    call(keyring, 'Init', [Threshold, Number], RootUrl).

-spec unlock(cds_keysharing:masterkey_share(), woody:url()) -> result().
unlock(Share, RootUrl) ->
    call(keyring, 'Unlock', [Share], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    call(keyring, 'Lock', [], RootUrl).

-spec rotate(woody:url()) -> result().
rotate(RootUrl) ->
    call(keyring, 'Rotate', [], RootUrl).

%% Identity document operations

-spec get_identity_doc(cds:token(), woody:url()) -> result().
get_identity_doc(Token, RootUrl) ->
    call(identity_doc, 'Get', [Token], RootUrl).

-spec put_identity_doc(identity_doc(), woody:url()) -> result().
put_identity_doc(IdentityDoc, RootUrl) ->
    call(identity_doc, 'Put', [IdentityDoc], RootUrl).

%%
%% Internals
%%

-spec call(atom(), atom(), list(), woody:url()) -> result().
call(ServiceCode, Function, Args, RootUrl) ->
    Request = {cds_thrift_services:thrift_service(ServiceCode), Function, Args},
    Path = genlib:to_binary(cds_thrift_services:service_path(ServiceCode)),
    TransportOpts = application:get_env(cds, net_opts, []),
    CallOpts = #{
        url => <<RootUrl/binary, Path/binary>>,
        event_handler => cds_woody_event_handler,
        transport_opts => TransportOpts
    },
    case woody_client:call(Request, CallOpts) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            throw(Exception)
    end.
