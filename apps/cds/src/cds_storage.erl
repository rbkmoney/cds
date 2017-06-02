-module(cds_storage).

-type limit() :: non_neg_integer() | undefined.
-type continuation() :: term().
-type timestamp() :: pos_integer().

-callback start() -> ok.
-callback get_token(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_cardholder_data(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.
-callback put_card_data(binary(), binary(), binary(), binary(), binary(), byte(), pos_integer()) -> ok.
-callback get_cvv(binary()) -> {ok, binary()} | {error, not_found}.
-callback update_cvv(binary(), binary(), byte()) -> ok | {error, not_found}.
-callback update_cardholder_data(binary(), binary(), binary(), byte()) -> ok | {error, not_found}.
-callback refresh_session_created_at(binary()) -> ok.
-callback delete_session(binary()) -> ok.

-callback get_sessions_created_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[term()], continuation()}} | no_return().
-callback get_sessions_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[term()], continuation()}} | no_return().
-callback get_tokens_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[term()], continuation()}} | no_return().
-callback get_sessions(limit(), continuation()) -> {ok, {[term()], continuation()}} | no_return().
-callback get_tokens(limit(), continuation()) -> {ok, {[term()], continuation()}} | no_return().

-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([get_sessions_created_between/4]).
-export([get_tokens_by_key_id_between/4]).
-export([get_sessions_by_key_id_between/4]).
-export([get_cvv/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/4]).
-export([refresh_session_created_at/1]).
-export([get_sessions/2]).
-export([get_tokens/2]).

-spec start() -> ok.
start() ->
    cds_backend:call(storage, start, []).

-spec get_token(binary()) -> binary() | no_return().
get_token(Hash) ->
    cds_backend:call(storage, get_token, [Hash]).

-spec get_cardholder_data(binary()) -> binary() | no_return().
get_cardholder_data(Token) ->
    cds_backend:call(storage, get_cardholder_data, [Token]).

-spec get_session_card_data(binary(), binary()) -> {binary(), binary()} | no_return().
get_session_card_data(Token, Session) ->
    cds_backend:call(storage, get_session_card_data, [Token, Session]).

-spec put_card_data(binary(), binary(), binary(), binary(), binary(), byte(), timestamp()) -> ok | no_return().
put_card_data(Token, Session, Hash, CardData, Cvv, KeyID, CreatedAt) ->
    cds_backend:call(storage, put_card_data, [Token, Session, Hash, CardData, Cvv, KeyID, CreatedAt]).

-spec delete_session(binary()) -> ok | no_return().
delete_session(Session) ->
    cds_backend:call(storage, delete_session, [Session]).

-spec get_sessions_created_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[term()], continuation()} | no_return().
get_sessions_created_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_sessions_created_between, [From, To, Limit, Continuation]).

-spec get_sessions_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[term()], continuation()} | no_return().
get_sessions_by_key_id_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_sessions_by_key_id_between, [From, To, Limit, Continuation]).

-spec get_tokens_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[term()], continuation()} | no_return().
get_tokens_by_key_id_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_tokens_by_key_id_between, [From, To, Limit, Continuation]).

-spec refresh_session_created_at(binary()) -> ok.
refresh_session_created_at(Session) ->
    cds_backend:call(storage, refresh_session_created_at, [Session]).

-spec get_sessions(limit(), continuation()) -> {[term()], continuation()} | no_return().
get_sessions(Limit, Continuation) ->
    cds_backend:call(storage, get_sessions, [Limit, Continuation]).

-spec get_tokens(limit(), continuation()) -> {[term()], continuation()} | no_return().
get_tokens(Limit, Continuation) ->
    cds_backend:call(storage, get_tokens, [Limit, Continuation]).

-spec get_cvv(binary()) -> binary().
get_cvv(Session) ->
    cds_backend:call(storage, get_cvv, [Session]).

-spec update_cvv(binary(), binary(), byte()) -> ok.
update_cvv(Session, Cvv, KeyID) ->
    cds_backend:call(storage, update_cvv, [Session, Cvv, KeyID]).

-spec update_cardholder_data(binary(), binary(), binary(), byte()) -> ok.
update_cardholder_data(Token, CardData, Hash, KeyID) ->
    cds_backend:call(storage, update_cardholder_data, [Token, CardData, Hash, KeyID]).

