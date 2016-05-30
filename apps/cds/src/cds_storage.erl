-module(cds_storage).

-callback start() -> ok.
-callback get_token(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_card_data(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.
-callback put_card_data(binary(), binary(), binary(), binary(), binary()) -> ok.
-callback delete_card_data(binary(), binary(), binary()) -> ok.
-callback delete_cvv(binary()) -> ok.

-export([start/0]).
-export([get_token/1]).
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/5]).
-export([delete_card_data/3]).
-export([delete_cvv/1]).

-spec start() -> ok.
start() ->
    cds_backend:call(storage, start, []).

-spec get_token(binary()) -> binary().
get_token(Hash) ->
    cds_backend:call(storage, get_token, [Hash]).

-spec get_card_data(binary()) -> binary().
get_card_data(Token) ->
    cds_backend:call(storage, get_card_data, [Token]).

-spec get_session_card_data(binary(), binary()) -> {binary(), binary()}.
get_session_card_data(Token, Session) ->
    cds_backend:call(storage, get_session_card_data, [Token, Session]).

-spec put_card_data(binary(), binary(), binary(), binary(), binary()) -> ok.
put_card_data(Token, Session, Hash, CardData, Cvv) ->
    cds_backend:call(storage, put_card_data, [Token, Session, Hash, CardData, Cvv]).

-spec delete_card_data(binary(), binary(), binary()) -> ok.
delete_card_data(Token, Hash, Session) ->
    cds_backend:call(storage, delete_card_data, [Token, Hash, Session]).

-spec delete_cvv(binary()) -> ok.
delete_cvv(Session) ->
    cds_backend:call(storage, delete_cvv, [Session]).
