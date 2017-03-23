-module(cds_session_cleaner).

-behaviour(gen_server).

%% api

-export([start_link/0]).

%% gen_server callbacks

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVICE, ?MODULE).
-define(DEFAULT_TIMEOUT, 3000).
-define(DEFAULT_BATCH_SIZE, 5000).
-define(DEFAULT_SESSION_LIFETIME, 3600).


-type state() :: #{
    timer := reference()
}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(_) -> {ok, state()}.

init(_Args) ->
    _ = lager:info("Starting session cleaner...", []),
    {ok, init_state(get_timeout())}.

handle_call(Request, From, State) ->
    _ = lager:error("Got unrecognized call from ~p: ", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    _ = lager:error("Got unrecognized cast: ", [Msg]),
    {noreply, State}.

handle_info(clean_timeout, State) ->
    _ = lager:info("Starting session cleaning", []),

    To = genlib_time:unow() - get_session_lifetime(),
    NewState = case clean_sessions(0, To, get_batch_size()) of
        {ok, done} ->
            _ = lager:info("Cleaned all the sessions"),
            State#{timer => set_timer(get_timeout())};
        {ok, more} ->
            _ = lager:info("Cleaned some sessions. Need to repeat"),
            State#{timer => set_timer(0)};
        {error, Error} ->
            _ = lager:error("Cleaning error: ~p", [Error]),
            State#{timer => set_timer(get_timeout())}
    end,
    {noreply, NewState};

handle_info(Msg, State) ->
    _ = lager:debug("Got unrecognized info: ", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec clean_sessions(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, done | more} | {error, Reason :: any()}.

clean_sessions(From, To, BatchSize) ->
    try
        Keys = cds_storage:get_sessions_created_between(From, To, BatchSize),
        KeysLength = length(Keys),
        _ = lager:info("Got ~p keys to clean", [KeysLength]),
        _ = [cds_storage:delete_session(ID) || ID <- Keys],
        case KeysLength of
            0 ->
                {ok, done};
            _ when is_integer(BatchSize), KeysLength < BatchSize ->
                {ok, done};
            _ ->
                {ok, more}
        end
    catch
        throw:Reason ->
            {error, Reason}
    end.

init_state(Timeout) -> #{
    timer => set_timer(Timeout)
}.

set_timer(Timeout) ->
    erlang:send_after(Timeout, self(), clean_timeout).

get_timeout() ->
    maps:get(timeout, get_config(), ?DEFAULT_TIMEOUT).

get_batch_size() ->
    maps:get(batch_size, get_config(), ?DEFAULT_BATCH_SIZE).

get_session_lifetime() ->
    maps:get(session_lifetime, get_config(), ?DEFAULT_SESSION_LIFETIME).

get_config() ->
    genlib_app:env(cds, session_cleaning, #{}).
