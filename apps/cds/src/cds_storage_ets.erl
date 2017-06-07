-module(cds_storage_ets).
-behaviour(cds_storage).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% cds_storage behaviour
-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([get_cvv/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/4]).
-export([refresh_session_created_at/1]).

-export([get_sessions_created_between/4]).
-export([get_tokens_by_key_id_between/4]).
-export([get_sessions_by_key_id_between/4]).
-export([get_sessions/2]).
-export([get_tokens/2]).

%% gen_server behaviour
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2]).
-export([code_change/3]).

-define(TOKEN_TABLE, cds_ets_storage_token).
-define(SESSION_TABLE, cds_ets_storage_session).


-define(CREATED_AT_INDEX, "created_at").
-define(KEY_ID_INDEX, "encoding_key_id").
-define(CARD_DATA_HASH_INDEX, "card_data_salted_hash").

%%
%% cds_storage behaviour
%%

-type limit() :: non_neg_integer() | undefined.
-type batch_response() :: {ok, {[term()], continuation()}}.
-type continuation() :: term().

-spec start() -> ok.

start() ->
    ChildSpec = #{
        id => ?MODULE,
        start => {gen_server, start_link, [?MODULE, [], []]}
    },
    {ok, _Child} = supervisor:start_child(cds, ChildSpec),
    ok.

-spec get_token(binary()) -> {ok, binary()} | {error, not_found}.

get_token(Hash) ->
    case get_keys_by_index_value(?TOKEN_TABLE, ?CARD_DATA_HASH_INDEX, Hash, 2) of
        {ok, {[Token], _}} ->
            {ok, Token};
        {ok, {[_ | _OtherTokens], _}} ->
            % This shouldnt happen, but we need to react somehow.
            error({<<"Hash collision detected">>, Hash});
        {ok, {[], _}} ->
            {error, not_found}
    end.

-spec get_cardholder_data(binary()) -> {ok, binary()} | {error, not_found}.

get_cardholder_data(Token) ->
    case get(?TOKEN_TABLE, Token) of
        {ok, V, _Index} ->
            {ok, V};
        Error ->
            Error
    end.

-spec get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.

get_session_card_data(Token, Session) ->
    case get(?SESSION_TABLE, Session) of
        {ok, Cvv, _} ->
            case get(?TOKEN_TABLE, Token) of
                {ok, CardData, _} ->
                    {ok, {CardData, Cvv}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec put_card_data(binary(), binary(), binary(), binary(), binary(), byte(), pos_integer()) -> ok.

put_card_data(Token, Session, Hash, CardData, Cvv, KeyID, CreatedAt) ->
    true = insert(
        ?TOKEN_TABLE,
        {
            Token,
            CardData
        },
        #{
            ?KEY_ID_INDEX => KeyID,
            ?CARD_DATA_HASH_INDEX => Hash
        }
    ),
    true = insert(
        ?SESSION_TABLE,
        {
            Session,
            Cvv
        },
        #{
            ?CREATED_AT_INDEX => CreatedAt,
            ?KEY_ID_INDEX => KeyID
        }
    ),
    ok.

-spec delete_session(binary()) -> ok.

delete_session(Session) ->
    true = ets:delete(?SESSION_TABLE, Session),
    ok.

-spec get_cvv(Session :: binary()) -> {ok, binary()} | {error, not_found}.

get_cvv(Session) ->
    case get(?SESSION_TABLE, Session) of
        {ok, Cvv, _Index} ->
            {ok, Cvv};
        Error ->
            Error
    end.

-spec update_cvv(Session :: binary(), NewCvv :: binary(), KeyID :: byte()) -> ok | {error, not_found}.

update_cvv(Session, NewCvv, KeyID) ->
    update(?SESSION_TABLE, Session, NewCvv, [{?KEY_ID_INDEX, KeyID}]).

-spec update_cardholder_data(binary(), binary(), binary(), byte()) -> ok | {error, not_found}.

update_cardholder_data(Token, NewCardData, NewHash, KeyID) ->
    update(?TOKEN_TABLE, Token, NewCardData, [{?KEY_ID_INDEX, KeyID}, {?CARD_DATA_HASH_INDEX, NewHash}]).

-spec refresh_session_created_at(binary()) -> ok.

refresh_session_created_at(Session) ->
    case get(?SESSION_TABLE, Session) of
        {ok, Value, _} ->
            update(?SESSION_TABLE, Session, Value, [{?CREATED_AT_INDEX, cds_utils:current_time()}]);
        {error, not_found} ->
            ok
    end.

-spec get_sessions_created_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response() | no_return().

get_sessions_created_between(From, To, Limit, _) ->
    get_keys_by_index_range(?SESSION_TABLE, ?CREATED_AT_INDEX, {From, To}, Limit).

-spec get_sessions_by_key_id_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response() | no_return().

get_sessions_by_key_id_between(From, To, Limit, _) ->
    get_keys_by_index_range(?SESSION_TABLE, ?KEY_ID_INDEX, {From, To}, Limit).

-spec get_tokens_by_key_id_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response() | no_return().

get_tokens_by_key_id_between(From, To, Limit, _) ->
    get_keys_by_index_range(?TOKEN_TABLE, ?KEY_ID_INDEX, {From, To}, Limit).

-spec get_sessions(limit(), continuation()) -> batch_response() | no_return().

get_sessions(Limit, Continuation) ->
    get_keys(?SESSION_TABLE, Limit, Continuation).

-spec get_tokens(limit(), continuation()) -> batch_response() | no_return().

get_tokens(Limit, Continuation) ->
    get_keys(?TOKEN_TABLE, Limit, Continuation).

%%
%% gen_server behaviour
%%

-type state() :: {}.

-spec init([]) -> {ok, state()}.

init([]) ->
    ?TOKEN_TABLE = ets:new(?TOKEN_TABLE, [named_table, public]),
    ?SESSION_TABLE = ets:new(?SESSION_TABLE, [named_table, public]),
    {ok, {}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

insert(Tab, {Key, Value}, Index) ->
    true = ets:insert(Tab, {Key, Value, Index}).

get(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_K, V, Index}] ->
            {ok, V, Index};
        [] ->
            {error, not_found}
    end.

get_keys(Tab, undefined, undefined) ->
    prepare_keys_result(ets:select(Tab, match_all()));

get_keys(Tab, Limit, undefined) when Limit > 0 ->
    prepare_keys_result(ets:select(Tab, match_all(), Limit));

get_keys(_, _, Continuation) ->
    prepare_keys_result(ets:select(Continuation)).

get_keys_by_index_value(Tab, IndexName, IndexValue, Limit) ->
    get_keys_by_index_range(Tab, IndexName, {IndexValue, IndexValue}, Limit).

get_keys_by_index_range(Tab, IndexName, {From, To}, Limit) ->
    Match = ets:select(Tab, match_all()),
    Result0 = [S ||
        {S, _, #{IndexName := IndexValue}} <- Match,
        IndexValue >= From,
        IndexValue =< To
    ],
    Result = case Limit of
        undefined -> Result0;
        _ -> lists:sublist(Result0, Limit)
    end,
    {ok, {Result, undefined}}.

update(Tab, Key, NewValue, NewIndexes) ->
    case get(Tab, Key) of
        {ok, _, OldIndex} ->
            true = insert(
                Tab,
                {Key, NewValue},
                update_indexes(OldIndex, NewIndexes)
            ),
            ok;
        Error ->
            Error
    end.

update_indexes(Old, Indexes) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            maps:put(K, V, Acc)
        end,
        Old,
        Indexes
    ).

match_all() ->
    ets:fun2ms(
        fun(V) -> V end
    ).

prepare_keys_result(Match) when is_list(Match) ->
    {ok, {[K || {K, _, _} <- Match], undefined}};
prepare_keys_result({Match, Continuation}) when is_list(Match) ->
    {ok, {[K || {K, _, _} <- Match], Continuation}};
prepare_keys_result('$end_of_table') ->
    {ok, {[], undefined}}.
