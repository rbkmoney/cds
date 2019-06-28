-module(cds_ct_stash).
-behaviour(gen_server).

-export([start/0]).
-export([stop/0]).
-export([put/2]).
-export([get/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(server_name, ?MODULE).
-define(call_timeout, 10000).

%%% API

-spec start() -> {ok, pid()} | {error, {already_started, pid()}}.

start() ->
    gen_server:start({local, ?server_name}, ?MODULE, [], []).

-spec stop() -> _.

stop() ->
    erlang:exit(erlang:whereis(?server_name), normal).

-spec put(term(), term()) -> ok.

put(Key, Value) ->
    call({put, Key, Value}).

-spec get(term()) -> term().

get(Key) ->
    call({get, Key}).

%%% gen_server callbacks

-spec init(term()) -> {ok, atom()}.

init(_) ->
    {ok, #{}}.

-spec handle_call(term(), pid(), atom()) -> {reply, atom(), atom()}.

handle_call({put, Key, Value}, _From, State) ->
    {reply, ok, State#{Key => Value}};
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State}.

-spec handle_cast(term(), atom()) ->  {noreply, atom()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), atom()) -> {noreply, atom()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), atom()) -> atom().

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, atom()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

call(Msg) ->
    gen_server:call(?server_name, Msg, ?call_timeout).
