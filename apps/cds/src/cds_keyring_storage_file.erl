-module(cds_keyring_storage_file).
-behaviour(cds_keyring_storage).

-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).
-export([delete/0]).

-define(DEFAULT_KEYRING_PATH, "/var/cds/keyring").
-define(DEFAULT_LOCK_PATH, "/var/run/cds/keyring.lock").


-spec get() -> {ok, binary()} | {error, not_found}.
get() ->
    Path = application:get_env(cds, keyring_location, ?DEFAULT_KEYRING_PATH),
    case file:read_file(Path) of
        {ok, Data} ->
            {ok, Data};
        {error, enoent} ->
            {error, not_found}
    end.


-spec put(binary()) -> ok.
put(Keyring) ->
    Path = application:get_env(cds, keyring_location, ?DEFAULT_KEYRING_PATH),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Keyring),
    ok.


-spec lock() -> ok | {error, locked}.
lock() ->
    Path = application:get_env(cds, keyring_lock_location, ?DEFAULT_LOCK_PATH),
    ok = filelib:ensure_dir(Path),
    case file:open(Path, [write, exclusive]) of
        {ok, _IoDevice} ->
            ok;
        {error, eexist} ->
            {error, locked}
    end.


-spec unlock() -> ok.
unlock() ->
    Path = application:get_env(cds, keyring_lock_location, ?DEFAULT_LOCK_PATH),
    ok = file:delete(Path),
    ok.


-spec delete() -> ok.
delete() ->
    Path = application:get_env(cds, keyring_location, ?DEFAULT_KEYRING_PATH),
    case file:delete(Path) of
        ok ->
            ok;
        {error, enoent} ->
            ok
    end.
