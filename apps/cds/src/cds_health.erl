-module(cds_health).

-export([keyring/0]).

-type version_key() :: binary(). % <<"keyring_version">>
-type info() :: #{
    version_key() := cds_keyring:version()
}.

-spec keyring() ->
    {ok, info()} | {error, 503, binary()}.

keyring() ->
    case cds_keyring:get_version() of
        undefined ->
            {error, 503, <<"Keyring is unavailable">>};
        Version ->
            {ok, #{<<"keyring_version">> => Version}}
    end.
