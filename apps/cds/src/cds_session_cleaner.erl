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

-type state() :: #{
    continuation := continuation()
}.

-type continuation() :: undefined | term().

-spec start_link() -> {ok, pid()} | {error, Reason :: any()}.
start_link() ->
    cds_periodic_job:start_link(?MODULE, []).

-spec init(_) -> {ok, non_neg_integer(), state()}.
init(_Args) ->
    _ = logger:info("Starting session cleaner...", []),
    {ok, get_interval(), #{continuation => undefined}}.

-spec handle_timeout(state()) -> {ok, done | more, state()} | {error, Reason :: any(), state()}.
handle_timeout(State = #{continuation := Continuation0}) ->
    _ = logger:info("Starting session cleaning", []),

    To = genlib_time:unow() - get_session_lifetime(),
    case clean_sessions(0, To, get_batch_size(), Continuation0) of
        {ok, done} ->
            _ = logger:info("Cleaned all the sessions"),
            {ok, done, State#{continuation => undefined}};
        {ok, more, Continuation} ->
            _ = logger:info("Cleaned some sessions. Need to repeat"),
            {ok, more, State#{continuation => Continuation}};
        {error, Error} ->
            _ = logger:error("Cleaning error: ~p", [Error]),
            {error, Error, State#{continuation => undefined}}
    end.

%% Internals

-spec clean_sessions(non_neg_integer(), non_neg_integer(), non_neg_integer() | undefined, continuation()) ->
    {ok, done} | {ok, more, continuation()} | {error, Reason :: any()}.
clean_sessions(From, To, BatchSize, Continuation0) ->
    try
        {Sessions, Continuation} = cds_card_storage:get_sessions_created_between(From, To, BatchSize, Continuation0),
        SessionsSize = length(Sessions),
        _ = logger:info("Got ~p sessions to clean", [SessionsSize]),
        _ = [
            begin
                _ = cds_card_storage:delete_session(ID),
                logger:debug("Deleted session with id = ~s", [cds_utils:encode_session(ID)])
            end
            || ID <- Sessions
        ],
        case BatchSize of
            undefined ->
                {ok, done};
            _ when SessionsSize < BatchSize ->
                {ok, done};
            _ ->
                {ok, more, Continuation}
        end
    catch
        throw:Reason ->
            {error, Reason};
        error:{woody_error, {_, Class, _}} = Reason when Class == resource_unavailable; Class == result_unknown ->
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
