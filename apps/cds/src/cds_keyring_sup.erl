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
            id => cds_keyring_manager,
            start => {cds_keyring_manager, start_link, []}
        },
        #{
            id => cds_keyring_initializer,
            start => {cds_keyring_initializer, start_link, []}
        },
        #{
            id => cds_keyring_rotator,
            start => {cds_keyring_rotator, start_link, []}
        },
        #{
            id => cds_keyring_unlocker,
            start => {cds_keyring_unlocker, start_link, []}
        },
        #{
            id => cds_keyring_rekeyer,
            start => {cds_keyring_rekeyer, start_link, []}
        }
    ],
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 1,
        period => 5
    },
    {ok, {SupFlags, ChildSpecs}}.

