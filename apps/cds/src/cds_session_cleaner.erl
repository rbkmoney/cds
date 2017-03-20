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
-define(DEFAULT_LIMIT, 5000).
-define(DEFAULT_SESSION_LIFETIME, 3600).


-type state() :: #{
    timer := reference()
}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(_) -> {ok, state()}.

init(_Args) ->
    CurrentConfig = #{
        timeout => get_timeout(),
        limit => get_limit(),
        session_lifetime => get_session_lifetime()
    },
    _ = lager:info("Starting session cleaner with config: ~p", [CurrentConfig]),
    {ok, init_state(get_timeout())}.

handle_call(Request, From, State) ->
    _ = lager:debug("Got unrecognized call from ~p: ", [From, Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    _ = lager:debug("Got unrecognized cast: ", [Msg]),
    {noreply, State}.

handle_info(clean_timeout, State) ->
    _ = lager:info("Starting session cleaning", []),

    To = genlib_time:unow() - get_session_lifetime(),
    NewState = case clean_sessions(0, To, get_limit()) of
        ok ->
            _ = lager:info("Cleaned all the sessions"),
            State#{timer => set_timer(get_timeout())};
        {ok, has_more} ->
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

clean_sessions(From, To, Limit) ->
    try
        Keys = cds_storage:get_sessions_keys(From, To, Limit),
        KeyLength = length(Keys),
        _ = lager:info("Got ~p keys to clean", [KeyLength]),
        case KeyLength of
            0 ->
                ok;
            _ ->
                _ = [cds_storage:delete_cvv(ID) || ID <- Keys],
                {ok, has_more}
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

get_limit() ->
    maps:get(limit, get_config(), ?DEFAULT_LIMIT).

get_session_lifetime() ->
    maps:get(session_lifetime, get_config(), ?DEFAULT_SESSION_LIFETIME).

get_config() ->
    genlib_app:env(cds, session_cleaner, #{}).
