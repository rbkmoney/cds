-module(cds_keyring_env_storage).
-behaviour(cds_keyring_storage).


-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).
-export([delete/0]).

-define(ENV_KEY, cds_keyring_env_storage).


-spec get() -> cds_backend:response(binary()) | cds_backend:error(not_found).
get() ->
    case application:get_env(cds, ?ENV_KEY) of
        {ok, Data} ->
            cds_backend:response(Data);
        undefined ->
            cds_backend:error(not_found)
    end.


-spec put(binary()) -> cds_backend:ok().
put(Keyring) ->
    ok = application:set_env(cds, ?ENV_KEY, Keyring),
    cds_backend:ok().


-spec lock() -> cds_backend:ok() | cds_backend:error(locked).
lock() ->
    cds_backend:ok().


-spec unlock() -> cds_backend:ok().
unlock() ->
    cds_backend:ok().


-spec delete() -> cds_backend:ok().
delete() ->
    case application:unset_env(cds, ?ENV_KEY) of
        ok ->
            cds_backend:ok()
    end.
