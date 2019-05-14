-module(cds_periodic_job).

-behaviour(gen_server).

%% callbacks
-type handler_state() :: term().
-type handler_interval() :: non_neg_integer().

-callback init(Args :: term()) -> {ok, handler_interval(), handler_state()}.
-callback handle_timeout(handler_state()) ->
    {ok, done, handler_state()} |
    {ok, more, handler_state()} |
    {error, Error :: term(), handler_state()}.

%% api

-export([start_link/2]).

%% gen_server callbacks

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT_MESSAGE, handler_timeout).

-type state() :: #{
    timer := reference(),
    handler := handler()
}.

-type handler() :: #{
    callback:= atom(),
    interval := handler_interval(),
    state := handler_state()
}.

-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.

start_link(Callback, Args) ->
    gen_server:start_link(?MODULE, [Callback, Args], []).

-spec init(_) -> {ok, state()}.

init([Callback, Args]) ->
    {ok, Interval, CallbackState} = Callback:init(Args),
    {ok, init_state(Interval, Callback, CallbackState)}.

-spec handle_call(term(), {pid(), term()}, state()) -> {noreply, state()}.

handle_call(Request, From, State) ->
    _ = logger:error("Got unrecognized call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(Msg, State) ->
    _ = logger:error("Got unrecognized cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(
    ?TIMEOUT_MESSAGE,
    State = #{handler := Handler = #{callback:= Callback, state := CallbackState0, interval := Interval}}
) ->
    NewState = case Callback:handle_timeout(CallbackState0) of
        {ok, done, CallbackState} ->
            State#{timer => set_timer(Interval), handler => Handler#{state => CallbackState}};
        {ok, more, CallbackState} ->
            State#{timer => set_timer(0), handler => Handler#{state => CallbackState}};
        {error, _Error, CallbackState} -> %% #TODO deal with error
            State#{timer => set_timer(Interval), handler => Handler#{state => CallbackState}}
    end,
    {noreply, NewState};

handle_info(Msg, State) ->
    _ = logger:debug("Got unrecognized info: ", [Msg]),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.

terminate(Reason, #{handler := #{callback := Callback, state := CallbackState}}) ->
    Exports = Callback:module_info(exports),
    case lists:member({terminate, 1}, Exports) of
        true ->
            _ = Callback:terminate(Reason, CallbackState),
            ok;
        false ->
            ok
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

init_state(Interval, Callback, CallbackState) ->
    #{
        timer => set_timer(Interval),
        handler => #{
            state => CallbackState,
            callback=> Callback,
            interval => Interval
        }
    }.

set_timer(Interval) ->
    erlang:send_after(Interval, self(), ?TIMEOUT_MESSAGE).

