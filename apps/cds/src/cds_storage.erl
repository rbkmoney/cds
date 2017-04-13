-module(cds_storage).

-callback start() -> ok.
-callback get_token(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_cardholder_data(binary()) -> {ok, binary()} | {error, not_found}.
-callback get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.
-callback put_card_data(binary(), binary(), binary(), binary(), binary(), byte(), pos_integer()) -> ok.
-callback delete_card_data(binary(), binary(), binary()) -> ok.
-callback delete_session(binary()) -> ok.
-callback get_sessions_created_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
-callback get_sessions_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
-callback get_tokens_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
-callback get_cvv(binary()) -> {ok, binary()} | {error, not_found}.
-callback update_cvv(binary(), binary(), byte()) -> ok | {error, not_found}.
-callback update_cardholder_data(binary(), binary(), byte()) -> ok | {error, not_found}.
-callback refresh_sessions() -> ok.

-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_card_data/3]).
-export([delete_session/1]).
-export([get_sessions_created_between/3]).
-export([get_tokens_by_key_id_between/3]).
-export([get_sessions_by_key_id_between/3]).
-export([get_cvv/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/3]).
-export([refresh_sessions/0]).

-type timestamp() :: pos_integer().

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

-spec delete_card_data(binary(), binary(), binary()) -> ok | no_return().
delete_card_data(Token, Hash, Session) ->
    cds_backend:call(storage, delete_card_data, [Token, Hash, Session]).

-spec delete_session(binary()) -> ok | no_return().
delete_session(Session) ->
    cds_backend:call(storage, delete_session, [Session]).

-spec get_sessions_created_between(
    non_neg_integer(),
    pos_integer(),
    pos_integer() | undefined
) -> [term()] | no_return().
get_sessions_created_between(From, To, Limit) when
    From =< To
->
    cds_backend:call(storage, get_sessions_created_between, [From, To, Limit]).

-spec get_sessions_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    pos_integer() | undefined
) -> [term()] | no_return().
get_sessions_by_key_id_between(From, To, Limit) when
    From =< To
->
    cds_backend:call(storage, get_sessions_by_key_id_between, [From, To, Limit]).

-spec get_tokens_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    pos_integer() | undefined
) -> [term()] | no_return().
get_tokens_by_key_id_between(From, To, Limit) when
    From =< To
->
    cds_backend:call(storage, get_tokens_by_key_id_between, [From, To, Limit]).

-spec refresh_sessions() -> ok | no_return().
refresh_sessions() ->
    cds_backend:call(storage, refresh_sessions, []).

-spec get_cvv(binary()) -> binary().
get_cvv(Session) ->
    cds_backend:call(storage, get_cvv, [Session]).

-spec update_cvv(binary(), binary(), byte()) -> ok.
update_cvv(Session, Cvv, KeyID) ->
    cds_backend:call(storage, update_cvv, [Session, Cvv, KeyID]).

-spec update_cardholder_data(binary(), binary(), byte()) -> ok.
update_cardholder_data(Token, CardData, KeyID) ->
    cds_backend:call(storage, update_cardholder_data, [Token, CardData, KeyID]).

