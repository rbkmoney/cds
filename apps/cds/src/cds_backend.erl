%% @doc Wrapper for all external backend calls.
%%
%% All backend behaviours must use only ok or {ok, term()} as response and
%% {error, term()} tuple for errors that are expected and which will be propagated to
%% corresponding thrift exception. All exceptions will be caught and logged
%% here and not propagated to interface handler
%%
-module(cds_backend).

-export([call/3]).

-spec call(atom(), atom(), list()) -> ok | term().
call(Key, Method, Args) ->
    {ok, Module} = application:get_env(cds, Key),
    try erlang:apply(Module, Method, Args) of
        ok ->
            ok;
        {ok, Return} ->
            Return;
        {error, Error} ->
            throw(Error)
    catch Class:Reason ->
        _ = lager:error(
            "~p (~p) ~p failed~nStacktrace:~s",
            [Key, Module, Method,
                lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
        handle_error(Class, Reason)
    end.

handle_error(throw, {noproc, Reason}) ->
    BinaryDescription = erlang:list_to_binary(io_lib:format("~9999p", [Reason])),
    woody_error:raise(system, {internal, resource_unavailable, BinaryDescription});
handle_error(error, timeout = Reason) ->
    BinaryDescription = erlang:list_to_binary(io_lib:format("~9999p", [Reason])),
    woody_error:raise(system, {internal, result_unknown, BinaryDescription});
handle_error(Class, Reason) ->
    exit({backend_error, {Class, Reason}}).
