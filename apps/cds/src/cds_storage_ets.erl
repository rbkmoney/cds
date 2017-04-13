-module(cds_storage_ets).
-behaviour(cds_storage).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-compile({no_auto_import, [get/1]}).

%% cds_storage behaviour
-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_card_data/3]).
-export([delete_session/1]).
-export([get_cvv/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/3]).
-export([get_sessions_created_between/3]).
-export([refresh_sessions/0]).
-export([get_tokens_by_key_id_between/3]).
-export([get_sessions_by_key_id_between/3]).


%% gen_server behaviour
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2]).
-export([code_change/3]).

-define(TOKEN_TABLE, cds_ets_storage_token).
-define(HASH_TABLE, cds_ets_storage_hash).
-define(SESSION_TABLE, cds_ets_storage_session).


-define(CREATED_AT_INDEX, "created_at").
-define(KEY_ID_INDEX, "encoding_key_id").

%%
%% cds_storage behaviour
%%

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
    case get(?HASH_TABLE, Hash) of
        {ok, V, _Index} ->
            {ok, V};
        Error ->
            Error
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
            ?KEY_ID_INDEX => KeyID
        }
    ),
    true = insert(?HASH_TABLE, {Hash, Token}),
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

-spec delete_card_data(binary(), binary(), binary()) -> ok.
delete_card_data(Token, Hash, Session) ->
    true = ets:delete(?TOKEN_TABLE, Token),
    true = ets:delete(?HASH_TABLE, Hash),
    true = ets:delete(?SESSION_TABLE, Session),
    ok.

-spec delete_session(binary()) -> ok.
delete_session(Session) ->
    true = ets:delete(?SESSION_TABLE, Session),
    ok.

-spec get_sessions_created_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
get_sessions_created_between(From, To, Limit) ->
    {ok, filter_by_index_between(?SESSION_TABLE, ?CREATED_AT_INDEX, {From, To}, Limit)}.

-spec get_sessions_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
get_sessions_by_key_id_between(From, To, Limit) ->
    {ok, filter_by_index_between(?SESSION_TABLE, ?KEY_ID_INDEX, {From, To}, Limit)}.

-spec get_tokens_by_key_id_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [term()]}.
get_tokens_by_key_id_between(From, To, Limit) ->
    {ok, filter_by_index_between(?TOKEN_TABLE, ?KEY_ID_INDEX, {From, To}, Limit)}.

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

-spec update_cardholder_data(Token :: binary(), NewCardData :: binary(), KeyID :: byte()) -> ok | {error, not_found}.
update_cardholder_data(Token, NewCardData, KeyID) ->
    update(?TOKEN_TABLE, Token, NewCardData, [{?KEY_ID_INDEX, KeyID}]).

-spec refresh_sessions() -> ok.
refresh_sessions() ->
    MatchSpec = ets:fun2ms(
        fun({Session, Cvv, _}) ->
            {Session, Cvv}
        end
    ),
    Result =  ets:select(?SESSION_TABLE, MatchSpec),
    [
        true = insert(
            ?SESSION_TABLE,
            {Session, Cvv},
            #{?CREATED_AT_INDEX => cds_utils:current_time()}
        )
            || {Session, Cvv} <- Result
    ],
    ok.

%%
%% gen_server behaviour
%%

init([]) ->
    ?HASH_TABLE = ets:new(?HASH_TABLE, [named_table, public]),
    ?TOKEN_TABLE = ets:new(?TOKEN_TABLE, [named_table, public]),
    ?SESSION_TABLE = ets:new(?SESSION_TABLE, [named_table, public]),
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

insert(Tab, {Key, Value}) ->
    insert(Tab, {Key, Value}, #{}).

insert(Tab, {Key, Value}, Index) ->
    true = ets:insert(Tab, {Key, Value, Index}).

get(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_K, V, Index}] ->
            {ok, V, Index};
        [] ->
            {error, not_found}
    end.

filter_by_index_between(Tab, IndexName, {From, To}, Limit) ->
    MatchSpec = ets:fun2ms(
        fun(V) -> V end
    ),
    Match = ets:select(Tab, MatchSpec),
    Result0 = [S ||
        {S, _, #{IndexName := IndexValue}} <- Match,
        IndexValue >= From,
        IndexValue =< To
    ],
    case Limit of
        undefined -> Result0;
        _ -> lists:sublist(Result0, Limit)
    end.

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
