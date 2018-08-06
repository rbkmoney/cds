-module(cds_hash).

-behaviour(supervisor).

-export([hash/2]).
-export([start_link/0]).
-export([init/1]).
-export([start_scrypt_port/0]).

-define(DEFAULT_HASH_PROC_COUNT, 4).

-spec hash(binary(), binary()) -> Hash :: binary().
hash(Plain, Salt) ->
    Pids = [Child || {_, Child, _, _} <- supervisor:which_children(?MODULE), is_pid(Child)],
    Pid = lists:nth(rand:uniform(length(Pids)), Pids),
    {N, R, P} = application:get_env(cds, scrypt_opts, {16384, 8, 1}),
    gen_server:call(Pid, {scrypt, Plain, Salt, N, R, P, 16}, infinity).

-spec start_link() -> {ok, pid()} | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Count = case erlang:system_info(logical_processors_available) of
        X when is_integer(X) ->
            X;
        _ ->
            ?DEFAULT_HASH_PROC_COUNT
    end,
    Specs = [#{id => I, start => {?MODULE, start_scrypt_port, []}} || I <- lists:seq(1, Count)],
    {ok, {{one_for_one, 1, 3}, Specs}}.

-spec start_scrypt_port() -> {ok, pid()}.
start_scrypt_port() ->
    gen_server:start_link(scrypt_port, [], []).
