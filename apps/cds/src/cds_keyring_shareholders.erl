-module(cds_keyring_shareholders).

%% API
-export([get_shareholders/0]).
-export_type([shareholder/0]).
-export_type([shareholders/0]).

-type shareholder() :: #{
    id := binary(),
    owner := binary(),
    public_key := binary()
}.
-type shareholders() :: list(shareholder()).

-spec get_shareholders() -> shareholders().
get_shareholders() ->
    genlib_app:env(cds, shareholders, []).