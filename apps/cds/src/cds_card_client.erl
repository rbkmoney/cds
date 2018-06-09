-module(cds_card_client).

-export([get_card_data/2]).
-export([get_session_card_data/3]).
-export([put_card_data/2]).
-export([put_card_data/3]).
-export([get_session_data/2]).

%%
%% Internal types
%%

-type result() :: cds_client_utils:result().
-type card_data() :: dmsl_cds_thrift:'CardData'().
-type session_data() :: dmsl_cds_thrift:'SessionData'() | undefined.

%%
%% API
%%

-spec get_card_data(cds:token(), woody:url()) -> result().
get_card_data(Token, RootUrl) ->
    cds_client_utils:call(card, 'GetCardData', [Token], RootUrl).

-spec get_session_card_data(cds:token(), cds:session(), woody:url()) -> result().
get_session_card_data(Token, Session, RootUrl) ->
    cds_client_utils:call(card, 'GetSessionCardData', [Token, Session], RootUrl).

-spec put_card_data(card_data(), woody:url()) -> result().
put_card_data(CardData, RootUrl) ->
    put_card_data(CardData, undefined, RootUrl).

-spec put_card_data(card_data(), session_data(), woody:url()) -> result().
put_card_data(CardData, SessionData, RootUrl) ->
    cds_client_utils:call(card, 'PutCardData', [CardData, SessionData], RootUrl).

-spec get_session_data(cds:session(), woody:url()) -> result().
get_session_data(Session, RootUrl) ->
    cds_client_utils:call(card, 'GetSessionData', [Session], RootUrl).

%%
%% Internals
%%
