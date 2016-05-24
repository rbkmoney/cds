-module(cds_keyring_storage_env).
-behaviour(cds_keyring_storage).


-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).
-export([delete/0]).

-define(ENV_KEY, cds_keyring_storage_env).


-spec get() -> {ok, binary()} | {error, not_found}.
get() ->
    case application:get_env(cds, ?ENV_KEY) of
        {ok, Data} ->
            {ok, Data};
        undefined ->
            {error, not_found}
    end.


-spec put(binary()) -> ok.
put(Keyring) ->
    ok = application:set_env(cds, ?ENV_KEY, Keyring),
    ok.


-spec lock() -> ok.
lock() ->
    ok.


-spec unlock() -> ok.
unlock() ->
    ok.


-spec delete() -> ok.
delete() ->
    ok = application:unset_env(cds, ?ENV_KEY),
    ok.
