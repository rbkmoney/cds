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
    gen_server:start_link({local, ?SERVICE}, ?MODULE, [], []).

-spec init(_) -> {ok | state()}.

init(_Args) ->
    lager:info("Starting cleaner..."),
    {ok, init_state(get_timeout())}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(clean_timeout, State) ->
    To = genlib_time:unow() - get_session_lifetime(),
    NewState = case clean_sessions(0, To, get_limit()) of
        ok ->
            State#{timer => set_timer(get_timeout())};
        {ok, has_more} ->
            State#{timer => set_timer(0)};
        {error, _Error} ->
            %% @TODO Report error
            State#{timer => set_timer(get_timeout())}
    end,
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

clean_sessions(From, To, Limit) ->
    try
        Keys = cds_storage:get_sessions_keys(From, To, Limit),
        case length(Keys) of
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
    erlang:send_after(Timeout, ?SERVICE, clean_timeout).

get_timeout() ->
    maps:get(timeout, get_config(), ?DEFAULT_TIMEOUT).

get_limit() ->
    maps:get(limit, get_config(), ?DEFAULT_LIMIT).

get_session_lifetime() ->
    maps:get(session_lifetime, get_config(), ?DEFAULT_SESSION_LIFETIME).

get_config() ->
    genlib_app:env(cds, session_cleaner, #{}).
