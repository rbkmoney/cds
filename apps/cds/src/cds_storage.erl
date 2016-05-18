-module(cds_storage).

-callback start() -> cds_backend:ok().
-callback get(atom(), binary()) -> cds_backend:response(binary()) | cds_backend:error(not_found).
-callback put(atom(), binary(), binary()) -> cds_backend:ok().
-callback delete(atom(), binary()) -> cds_backend:ok().

-export([start/0]).
-export([get/2]).
-export([put/3]).
-export([delete/2]).

-spec start() -> cds_backend:ok().
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
