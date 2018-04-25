-module(cds_storage).

%% TODO remove circular dependencies in types (cds:hash, cds:token etc.)

-type limit() :: non_neg_integer() | undefined.
-type continuation() :: term().
-type timestamp() :: pos_integer().

-callback start() -> ok.
-callback get_token(cds:hash()) -> {ok, cds:token()} | {error, not_found}.
-callback get_cardholder_data(cds:token()) -> {ok, cds:ciphertext()} | {error, not_found}.
-callback get_session_card_data(cds:token(), cds:session()) ->
    {ok, {CardData :: cds:ciphertext(), SessionData :: cds:ciphertext()}} | {error, not_found}.
-callback put_card_data(
    cds:token(),
    cds:session(),
    cds:hash(),
    CardData :: cds:ciphertext(),
    SessionData :: cds:ciphertext(),
    cds_keyring:key_id(),
    timestamp()
) -> ok.
-callback get_session_data(cds:session()) ->
    {ok, SessionData :: cds:ciphertext()} | {error, not_found}.
-callback update_cardholder_data(cds:token(), CardData :: cds:ciphertext(), cds:hash(), cds_keyring:key_id()) ->
    ok | {error, not_found}.
-callback update_session_data(
    cds:session(),
    cds:ciphertext() | cds:ciphertext(),
    cds_keyring:key_id()
) ->
    ok | {error, not_found}.
-callback refresh_session_created_at(cds:session()) -> ok.
-callback delete_session(binary()) -> ok.

-callback get_sessions_created_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[cds:session()], continuation()}} | no_return().
-callback get_sessions_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[cds:session()], continuation()}} | no_return().
-callback get_tokens_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    limit(),
    continuation()
) -> {ok, {[cds:token()], continuation()}} | no_return().
-callback get_sessions(limit(), continuation()) -> {ok, {[cds:session()], continuation()}} | no_return().
-callback get_sessions_info(limit(), continuation()) ->
    {ok, {[{cds:session(), Info :: map()}], continuation()}} | no_return().
-callback get_tokens(limit(), continuation()) -> {ok, {[cds:token()], continuation()}} | no_return().

-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([get_session_data/1]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([get_sessions_created_between/4]).
-export([get_tokens_by_key_id_between/4]).
-export([get_sessions_by_key_id_between/4]).
-export([update_cardholder_data/4]).
-export([update_session_data/3]).
-export([refresh_session_created_at/1]).
-export([get_sessions/2]).
-export([get_sessions_info/2]).
-export([get_tokens/2]).

-spec start() -> ok.
start() ->
    cds_backend:call(storage, start, []).

-spec get_token(cds:hash()) -> cds:token() | no_return().
get_token(Hash) ->
    cds_backend:call(storage, get_token, [Hash]).

-spec get_cardholder_data(cds:token()) -> cds:ciphertext() | no_return().
get_cardholder_data(Token) ->
    cds_backend:call(storage, get_cardholder_data, [Token]).

-spec get_session_card_data(cds:token(), cds:session()) ->
    {CardData :: cds:ciphertext(), SessionData :: cds:ciphertext()} | no_return().
get_session_card_data(Token, Session) ->
    cds_backend:call(storage, get_session_card_data, [Token, Session]).

-spec put_card_data(
    cds:token(),
    cds:session(),
    cds:hash(),
    CardData :: cds:ciphertext(),
    SessionData :: cds:ciphertext(),
    cds_keyring:key_id(),
    timestamp()
) -> ok | no_return().
put_card_data(Token, Session, Hash, CardData, SessionData, KeyID, CreatedAt) ->
    cds_backend:call(storage, put_card_data, [Token, Session, Hash, CardData, SessionData, KeyID, CreatedAt]).

-spec get_session_data(cds:session()) -> cds:ciphertext() | no_return().
get_session_data(Session) ->
    cds_backend:call(storage, get_session_data, [Session]).

-spec delete_session(cds:session()) -> ok | no_return().
delete_session(Session) ->
    cds_backend:call(storage, delete_session, [Session]).

-spec get_sessions_created_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:session()], continuation()} | no_return().
get_sessions_created_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_sessions_created_between, [From, To, Limit, Continuation]).

-spec get_sessions_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:session()], continuation()} | no_return().
get_sessions_by_key_id_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_sessions_by_key_id_between, [From, To, Limit, Continuation]).

-spec get_tokens_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:token()], continuation()} | no_return().
get_tokens_by_key_id_between(From, To, Limit, Continuation) when
    From =< To
->
    cds_backend:call(storage, get_tokens_by_key_id_between, [From, To, Limit, Continuation]).

-spec refresh_session_created_at(cds:session()) -> ok.
refresh_session_created_at(Session) ->
    cds_backend:call(storage, refresh_session_created_at, [Session]).

-spec get_sessions(limit(), continuation()) -> {[cds:session()], continuation()} | no_return().
get_sessions(Limit, Continuation) ->
    cds_backend:call(storage, get_sessions, [Limit, Continuation]).

-spec get_sessions_info(limit(), continuation()) -> {[{cds:session(), Info :: map()}], continuation()} | no_return().
get_sessions_info(Limit, Continuation) ->
    cds_backend:call(storage, get_sessions_info, [Limit, Continuation]).

-spec get_tokens(limit(), continuation()) -> {[cds:token()], continuation()} | no_return().
get_tokens(Limit, Continuation) ->
    cds_backend:call(storage, get_tokens, [Limit, Continuation]).

-spec update_cardholder_data(cds:token(), cds:ciphertext(), cds:hash(), cds_keyring:key_id()) -> ok.
update_cardholder_data(Token, CardData, Hash, KeyID) ->
    cds_backend:call(storage, update_cardholder_data, [Token, CardData, Hash, KeyID]).

-spec update_session_data(
    cds:session(),
    cds:ciphertext(),
    cds_keyring:key_id()
) -> ok.
update_session_data(Session, SessionData, KeyID) ->
    cds_backend:call(storage, update_session_data, [Session, SessionData, KeyID]).