-module(cds_keyring_storage).

-callback get() -> cds_backend:response(binary()) | cds_backend:error(not_found).
-callback put(Keyring :: binary()) -> cds_backend:ok().
-callback lock() -> cds_backend:ok() | cds_backend:error(locked).
-callback unlock() -> cds_backend:ok().
-callback delete() -> cds_backend:ok().

-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).
-export([delete/0]).

-spec get() -> binary().
get() ->
	cds_backend:call(keyring_storage, get, []).

-spec put(binary()) -> ok.
put(Keyring) ->
	cds_backend:call(keyring_storage, put, [Keyring]).

-spec lock() -> ok.
lock() ->
	cds_backend:call(keyring_storage, lock, []).

-spec unlock() -> ok.
unlock() ->
	cds_backend:call(keyring_storage, unlock, []).

-spec delete() -> ok.
delete() ->
	cds_backend:call(keyring_storage, delete, []).
