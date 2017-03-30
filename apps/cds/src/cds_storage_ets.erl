-module(cds_storage_ets).
-behaviour(cds_storage).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% cds_storage behaviour
-export([start/0]).
-export([get_token/1]).
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/6]).
-export([delete_card_data/3]).
-export([delete_session/1]).
-export([get_sessions_created_between/3]).
-export([refresh_sessions/0]).

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
    case ets:lookup(?HASH_TABLE, Hash) of
        [{Hash, Token}] ->
            {ok, Token};
        [] ->
            {error, not_found}
    end.

-spec get_card_data(binary()) -> {ok, binary()} | {error, not_found}.
get_card_data(Token) ->
    case ets:lookup(?TOKEN_TABLE, Token) of
        [{Token, CardData}] ->
            {ok, CardData};
        [] ->
            {error, not_found}
    end.

-spec get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.
get_session_card_data(Token, Session) ->
    case ets:lookup(?SESSION_TABLE, Session) of
        [{Session, Cvv, _CreatedAt}] ->
            case ets:lookup(?TOKEN_TABLE, Token) of
                [{Token, CardData}] ->
                    {ok, {CardData, Cvv}};
                [] ->
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

-spec put_card_data(binary(), binary(), binary(), binary(), binary(), pos_integer()) -> ok.
put_card_data(Token, Session, Hash, CardData, Cvv, CreatedAt) ->
    true = ets:insert(?TOKEN_TABLE, {Token, CardData}),
    true = ets:insert(?HASH_TABLE, {Hash, Token}),
    true = ets:insert(?SESSION_TABLE, {Session, Cvv, CreatedAt}),
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
) -> {ok, [binary()]}.
get_sessions_created_between(From, To, Limit) ->
    MatchSpec =  ets:fun2ms(
        fun({Session, _, CreatedAt}) when
            CreatedAt >= From,
            CreatedAt =< To
        ->
            Session
        end
    ),
    Result = case Limit of
        undefined -> ets:select(?SESSION_TABLE, MatchSpec);
        Limit -> ets:select(?SESSION_TABLE, MatchSpec, Limit)
    end,

    case Result of
        Match when is_list(Match) ->
            {ok, Match};
        {Match, _Continuation} ->
            {ok, Match};
        '$end_of_table' ->
            {ok, []}
    end.

-spec refresh_sessions() -> ok.
refresh_sessions() ->
    MatchSpec = ets:fun2ms(
        fun({Session, Cvv, _}) ->
            {Session, Cvv}
        end
    ),
    Result =  ets:select(?SESSION_TABLE, MatchSpec),
    [true = ets:insert(?SESSION_TABLE, {Session, Cvv, cds_utils:current_time()})
    || {Session, Cvv} <- Result],
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
