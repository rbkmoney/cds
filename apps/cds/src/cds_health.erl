-module(cds_health).

-export([keyring/0]).

% <<"keyring_version">>
-type version_key() :: binary().
-type info() :: #{
    version_key() := cds_keyring:version()
}.

-spec keyring() -> {passing, info()} | {critical, binary()}.
keyring() ->
    case cds_keyring:get_version() of
        undefined ->
            {critical, <<"Keyring is unavailable">>};
        Version ->
            {passing, #{<<"version">> => Version}}
    end.
