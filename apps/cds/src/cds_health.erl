-module(cds_health).

-export([keyring/0]).

-type version_key() :: binary(). % <<"keyring_version">>
-type info() :: #{
    version_key() := cds_keyring:version()
}.

-spec keyring() ->
    {passing, info()} | {critical, binary()}.

keyring() ->
    case cds_keyring:get_version() of
        undefined ->
            {critical, <<"Keyring is unavailable">>};
        Version ->
            {passing, #{<<"version">> => Version}}
    end.
