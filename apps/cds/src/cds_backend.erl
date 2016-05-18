%% @doc Wrapper for all external backend calls.
%%
%% All backend behaviours must use only ok() or response() types as response and
%% error() type for errors that are expected and which will be propagated to
%% corresponding thrift exception. All exceptions will be caught and logged
%% here and not propagated to interface handler
%%
-module(cds_backend).

-export([call/3]).

-export([ok/0]).
-export([response/1]).
-export([error/1]).

-export_types([ok/0, response/1, error/1]).

-type ok() :: ok.
-type response(Response) :: {ok, Response}.
-type error(Reason) :: {error, Reason}.

-spec call(atom(), atom(), list()) -> ok() | term().
call(Key, Method, Args) ->
    Module = cds_config:get(Key),
    try erlang:apply(Module, Method, Args) of
        ok ->
            ok;
        {ok, Return} ->
            Return;
        {error, Error} ->
            throw(Error)
    catch Class:Reason ->
        lager:error("~p (~p) ~p failed with ~p ~p", [Key, Module, Method, Class, Reason]),
        exit(backend_error)
    end.

-spec ok() -> ok().
ok() ->
    ok.

-spec response(Response) -> response(Response) when Response :: term().
response(Term) ->
    {ok, Term}.

-spec error(Reason) -> error(Reason) when Reason :: term().
error(Term) ->
    {error, Term}.
