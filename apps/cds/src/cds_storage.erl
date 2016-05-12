-module(cds_storage).

-callback get(atom(), binary()) -> cds_backend:response(binary()) | cds_backend:error(not_found).
-callback put(atom(), binary(), binary()) -> cds_backend:ok().
-callback delete(atom(), binary()) -> cds_backend:ok().

-export([get/2]).
-export([put/3]).
-export([delete/2]).

-spec get(atom(), binary()) -> binary().
get(Type, Key) ->
    cds_backend:call(storage, get, [Type, Key]).

-spec put(atom(), binary(), binary()) -> ok.
put(Type, Key, Data) ->
    cds_backend:call(storage, put, [Type, Key, Data]).

-spec delete(atom(), binary()) -> ok.
delete(Type, Key) ->
    cds_backend:call(storage, delete, [Type, Key]).
