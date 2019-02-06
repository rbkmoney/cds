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
        lager:error(
            "~s ~p (~p) ~p failed~nStacktrace:~s",
            [cds_utils:get_rpc_id(), Key, Module, Method,
                lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
        exit(backend_error)
    end.
