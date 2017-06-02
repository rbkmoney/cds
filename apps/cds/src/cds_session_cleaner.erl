-module(cds_session_cleaner).

-behaviour(cds_periodic_job).

%% api

-export([start_link/0]).

%% cds_periodic_job callbacks

-export([init/1]).
-export([handle_timeout/1]).

-define(DEFAULT_INTERVAL, 3000).
-define(DEFAULT_BATCH_SIZE, 5000).
-define(DEFAULT_SESSION_LIFETIME, 3600).

-spec start_link() -> {ok, pid()} | {error, Reason :: any()}.

start_link() ->
    cds_periodic_job:start_link(?MODULE, []).

-spec init(_) -> {ok, non_neg_integer(), undefined}.

init(_Args) ->
    _ = lager:info("Starting session cleaner...", []),
    {ok, get_interval(), undefined}.

-spec handle_timeout(any()) -> {ok, done | more, any()} | {error, Reason :: any(), any()}.

handle_timeout(State) ->
    _ = lager:info("Starting session cleaning", []),

    To = genlib_time:unow() - get_session_lifetime(),
    case clean_sessions(0, To, get_batch_size()) of
        {ok, done} ->
            _ = lager:info("Cleaned all the sessions"),
            {ok, done, State};
        {ok, more} ->
            _ = lager:info("Cleaned some sessions. Need to repeat"),
            {ok, more, State};
        {error, Error} ->
            _ = lager:error("Cleaning error: ~p", [Error]),
            {error, Error, State}
    end.

-spec clean_sessions(non_neg_integer(), non_neg_integer(), non_neg_integer() | undefined) ->
    {ok, done | more} | {error, Reason :: any()}.

clean_sessions(From, To, BatchSize) ->
    try
        Sessions = cds:get_sessions_created_between(From, To, BatchSize),
        SessionsSize = length(Sessions),
        _ = lager:info("Got ~p sessions to clean", [SessionsSize]),
        _ = [
            begin
            _ = cds:delete_session(ID),
            lager:debug("Deleted session ~p", [ID])
            end
        || ID <- Sessions],
        case BatchSize of
            undefined ->
                {ok, done};
            _ when SessionsSize < BatchSize ->
                {ok, done};
            _ ->
                {ok, more}
        end
    catch
        throw:Reason ->
            {error, Reason}
    end.

%% Internals

get_interval() ->
    maps:get(interval, get_config(), ?DEFAULT_INTERVAL).

get_batch_size() ->
    maps:get(batch_size, get_config(), ?DEFAULT_BATCH_SIZE).

get_session_lifetime() ->
    maps:get(session_lifetime, get_config(), ?DEFAULT_SESSION_LIFETIME).

get_config() ->
    genlib_app:env(cds, session_cleaning, #{}).
