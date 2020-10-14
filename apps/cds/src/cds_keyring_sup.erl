-module(cds_keyring_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(_) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_) ->
    ChildSpecs = [
        #{
            id => cds_keyring,
            start => {cds_keyring, start_link, []}
        }
    ],
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    {ok, {SupFlags, ChildSpecs}}.
