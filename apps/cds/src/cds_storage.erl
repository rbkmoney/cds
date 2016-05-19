-module(cds_storage).

-callback start() -> ok.
-callback get(atom(), binary()) -> {ok, binary()} | {error, not_found}.
-callback put(atom(), binary(), binary()) -> ok.
-callback delete(atom(), binary()) -> ok.

-export([start/0]).
-export([get/2]).
-export([put/3]).
-export([delete/2]).

-spec start() -> ok.
start() ->
    cds_backend:call(storage, start, []).

-spec get(atom(), binary()) -> binary().
get(Type, Key) ->
    cds_backend:call(storage, get, [Type, Key]).

-spec put(atom(), binary(), binary()) -> ok.
put(Type, Key, Data) ->
    cds_backend:call(storage, put, [Type, Key, Data]).

-spec delete(atom(), binary()) -> ok.
delete(Type, Key) ->
    cds_backend:call(storage, delete, [Type, Key]).
