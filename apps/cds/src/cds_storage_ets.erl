-module(cds_storage_ets).
-behaviour(cds_storage).
-behaviour(gen_server).

%% cds_storage behaviour
-export([start/0]).
-export([get_token/1]).
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/5]).
-export([delete_card_data/3]).
-export([delete_cvv/1]).

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
        [{Session, Cvv}] ->
            case ets:lookup(?TOKEN_TABLE, Token) of
                [{Token, CardData}] ->
                    {ok, {CardData, Cvv}};
                [] ->
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

-spec put_card_data(binary(), binary(), binary(), binary(), binary()) -> ok.
put_card_data(Token, Session, Hash, CardData, Cvv) ->
    true = ets:insert(?TOKEN_TABLE, {Token, CardData}),
    true = ets:insert(?HASH_TABLE, {Hash, Token}),
    true = ets:insert(?SESSION_TABLE, {Session, Cvv}),
    ok.

-spec delete_card_data(binary(), binary(), binary()) -> ok.
delete_card_data(Token, Hash, Session) ->
    true = ets:delete(?TOKEN_TABLE, Token),
    true = ets:delete(?HASH_TABLE, Hash),
    true = ets:delete(?SESSION_TABLE, Session),
    ok.

-spec delete_cvv(binary()) -> ok.
delete_cvv(Session) ->
    true = ets:delete(?SESSION_TABLE, Session),
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
