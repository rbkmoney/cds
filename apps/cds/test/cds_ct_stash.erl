-module(cds_ct_stash).

-export([start/0]).
-export([stop/1]).
-export([put/3]).
-export([get/2]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-spec start() -> pid().

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

-spec stop(pid()) -> _.

stop(Pid) ->
    erlang:exit(Pid, normal).

-spec put(pid(), term(), term()) -> ok.

put(Pid, Key, Value) ->
    gen_server:call(Pid, {put, Key, Value}, 5000).

-spec get(pid(), term()) -> term().

get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}, 5000).

%%

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
