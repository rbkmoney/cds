-module(cds_keyring_storage_file).
-behaviour(cds_keyring_storage).

-export([create/1]).
-export([read/0]).
-export([update/1]).
-export([delete/0]).

-define(DEFAULT_KEYRING_PATH, "/var/cds/keyring").

-spec create(binary()) -> ok | {error, already_exists}.
create(Keyring) ->
    Path = application:get_env(cds, keyring_path, ?DEFAULT_KEYRING_PATH),
    case filelib:is_regular(Path) of
        false ->
            ok = filelib:ensure_dir(Path),
            ok = atomic_write(Path, Keyring);
        true ->
            {error, already_exists}
    end.

-spec read() -> {ok, binary()} | {error, not_found}.
read() ->
    Path = application:get_env(cds, keyring_path, ?DEFAULT_KEYRING_PATH),
    case file:read_file(Path) of
        {ok, Data} ->
            {ok, Data};
        {error, enoent} ->
            {error, not_found}
    end.

-spec update(binary()) -> ok.
update(Keyring) ->
    Path = application:get_env(cds, keyring_path, ?DEFAULT_KEYRING_PATH),
    ok = filelib:ensure_dir(Path),
    ok = atomic_write(Path, Keyring).

-spec delete() -> ok.
delete() ->
    Path = application:get_env(cds, keyring_path, ?DEFAULT_KEYRING_PATH),
    case file:delete(Path) of
        ok ->
            ok;
        {error, enoent} ->
            ok
    end.

atomic_write(Path, Keyring) ->
    TmpPath = tmp_keyring_path(Path),
    ok = file:write_file(TmpPath, Keyring),
    file:rename(TmpPath, Path).

tmp_keyring_path(Path) ->
    Path ++ "_tmp".
