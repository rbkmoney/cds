-module(cds_keyring_storage).

-callback get() -> {ok, Keyring :: binary()} | {error, Reason :: term()}.
-callback put(Keyring :: binary()) -> ok | {error, Reason :: term()}.
-callback lock() -> ok | {error, Reason :: term()}.
-callback unlock() -> ok | {error, Reason :: term()}.

-export([get/0]).
-export([put/1]).
-export([lock/0]).
-export([unlock/0]).


get() ->
	call_backend(get, []).

put(Keyring) ->
	call_backend(put, [Keyring]).

lock() ->
	call_backend(lock, []).

unlock() ->
	call_backend(unlock, []).


call_backend(Method, Args) ->
	case application:get_env(cds, keyring_storage) of
		{ok, Backend} ->
			erlang:apply(Backend, Method, Args);
		undefined ->
			{error, {not_configured, keyring_storage}}
	end.