-module(cds_keyring_file_storage).
-behaviour(cds_keyring_storage).


-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).


-spec get() -> {ok, binary()} | {error, term()}.
get() ->
	add_config(fun get/1, [], keyring_location).

get(Path) ->
	file:read_file(Path).


-spec put(binary()) -> ok | {error, term()}.
put(Keyring) ->
	add_config(fun put/2, [Keyring], keyring_location).

put(Path, Keyring) ->
	file:write_file(Path, Keyring).


-spec lock() -> ok | {error, term()}.
lock() ->
	add_config(fun lock/1, [], keyring_lock_location).

lock(Path) ->
	case file:open(Path, [write, exclusive]) of
		{ok, _IoDevice} ->
			ok;
		{error, eexist} ->
			{error, locked};
		{error, OtherReason} ->
			{error, OtherReason}
	end.


-spec unlock() -> ok | {error, term()}.
unlock() ->
	add_config(fun unlock/1, [], keyring_lock_location).

unlock(Path) ->
	file:delete(Path).


-spec add_config(function(), list(), atom()) -> any().
add_config(Method, Args, ConfigKey) ->
	case application:get_env(cds, ConfigKey) of
		{ok, Config} ->
			erlang:apply(Method, [Config | Args]);
		undefined ->
			{error, {not_configured, ConfigKey}}
	end.