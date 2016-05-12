-module(cds_keyring_file_storage).
-behaviour(cds_keyring_storage).


-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).
-export([delete/0]).


-spec get() -> cds_backend:response(binary()) | cds_backend:error(not_found).
get() ->
    Path = cds_config:get(keyring_location),
    case file:read_file(Path) of
        {ok, Data} ->
            cds_backend:response(Data);
        {error, enoent} ->
            cds_backend:error(not_found)
    end.


-spec put(binary()) -> cds_backend:ok().
put(Keyring) ->
    Path = cds_config:get(keyring_location),
    ok = file:write_file(Path, Keyring),
    cds_backend:ok().


-spec lock() -> cds_backend:ok() | cds_backend:error(locked).
lock() ->
    Path = cds_config:get(keyring_lock_location),
    case file:open(Path, [write, exclusive]) of
        {ok, _IoDevice} ->
            cds_backend:ok();
        {error, eexist} ->
            cds_backend:error(locked)
    end.


-spec unlock() -> cds_backend:ok().
unlock() ->
    Path = cds_config:get(keyring_lock_location),
    ok = file:delete(Path),
    cds_backend:ok().


-spec delete() -> cds_backend:ok().
delete() ->
    Path = cds_config:get(keyring_location),
    case file:delete(Path) of
        ok ->
            cds_backend:ok();
        {error, enoent} ->
            cds_backend:error(not_found)
    end.
