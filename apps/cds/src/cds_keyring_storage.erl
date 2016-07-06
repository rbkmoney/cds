-module(cds_keyring_storage).

-callback create(binary()) -> ok | {error, already_exists}.
-callback read() -> {ok, binary()} | {error, not_found}.
-callback update(binary()) -> ok.
-callback delete() -> ok.

-export([create/1]).
-export([read/0]).
-export([update/1]).
-export([delete/0]).


-spec create(binary()) -> ok.
create(Keyring) ->
    cds_backend:call(keyring_storage, create, [Keyring]).

-spec read() -> binary().
read() ->
    cds_backend:call(keyring_storage, read, []).

-spec update(binary()) -> ok.
update(Keyring) ->
    cds_backend:call(keyring_storage, update, [Keyring]).

-spec delete() -> ok.
delete() ->
    cds_backend:call(keyring_storage, delete, []).
